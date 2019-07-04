package com.worthlesscog.video

import java.io.File

import com.worthlesscog.video.MkvToolNix.{CODEC_VOBSUB, TYPE_SUBTITLE}

object ForcedSubTrim {

    object Op extends Enumeration {
        val describe, noop, scan, trim = Value
    }

    val MKV = """^(.*\.mkv)$""".r
    val TRACK_ID = """^(\d{1,2})$""".r

    var op = Op.noop
    var paths = List.empty[String]
    var trackId = 0

    def main(args: Array[String]): Unit =
        parseArgs(args toList) match {
            case Left(oops)         => info(oops)
            case Right(Op.describe) => walkPaths(paths, describeTracks)
            case Right(Op.scan)     => walkPaths(paths, scanSubtitles)
            case Right(Op.trim)     => trim(trackId, paths)
        }

    private def parseArgs(args: List[String]): Maybe[Op.Value] =
        if (args isEmpty) {
            if (paths isEmpty)
                Left("Path?\n")
            else if (op == Op.noop)
                Left("Operation?\n")
            else
                Right(op)
        } else args match {
            case "-describe" :: tail =>
                op = Op.describe
                parseArgs(tail)

            case "-scan" :: tail =>
                op = Op.scan
                parseArgs(tail)

            case "-trim" :: TRACK_ID(id) :: tail =>
                trackId = id.toInt
                op = Op.trim
                parseArgs(tail)

            case path :: tail =>
                paths = path :: paths
                parseArgs(tail)
        }

    def walkPaths(paths: List[String], v: File => Unit) = {
        paths foreach { p =>
            walk(new File(p), filter(v))
        }
    }

    def filter(v: File => Unit)(f: File) = f.getName match {
        case MKV(_) => v(f)
        case _      =>
    }

    def describeTracks(f: File) {
        info(f toString)
        MkvToolNix.trackList(f) foreach { t =>
            info("\t" + t.description)
        }
    }

    def scanSubtitles(f: File) {
        val ss = MkvToolNix.trackList(f) |> ofType(TYPE_SUBTITLE) |> ofCodec(CODEC_VOBSUB)
        if (ss nonEmpty) {
            info(f toString)
            MkvToolNix.demux(f, ss)
            ss foreach { t =>
                info("\t" + t.description)
                identifyForcedSubs(f, t)
            }
        }
    }

    def ofType(kind: String)(ts: List[Track]) =
        ts filter { _.`type` == kind }

    def ofCodec(codec: String)(ts: List[Track]) =
        ts filter { _.codec == codec }

    def identifyForcedSubs(f: File, t: Track) =
        read(f, t) match {
            case Right(ss) =>
                ss foreach { s =>
                    if (s isForced)
                        info("\t\t" + s.timeStamp)
                }

            case Left(oops) =>
                info("\t\t" + oops)
        }

    def read(f: File, t: Track) = {
        def read(s: SubtitleStream, ss: List[SubPictureUnit]): Maybe[List[SubPictureUnit]] = {
            s.next match {
                case Right(None)    => Right(ss reverse)
                case Right(Some(p)) => read(s, p :: ss)
                case Left(oops)     => Left(oops)
            }
        }

        read(VobSub.open(extractFile(f, t)), Nil)
    }

    def extractFile(f: File, t: Track) =
        new File(MkvToolNix.extractedTrackName(f, t))

    def extractIndex(f: File, t: Track) =
        new File(MkvToolNix.extractedTrackIndex(f, t))

    def trim(trackId: Int, paths: List[String]) =
        paths foreach { p =>
            val f = new File(p)
            if (f.exists && f.isFile) {
                val ss = MkvToolNix.trackList(f) |> ofType(TYPE_SUBTITLE) |> ofCodec(CODEC_VOBSUB)
                ss find { _.id == trackId } foreach trimTrack(f)
            }
        }

    def trimTrack(f: File)(t: Track) {
        info(f toString)
        MkvToolNix.demux(f, List(t))
        read(f, t) match {
            case Right(spus) =>
                val forced = spus filter { _.isForced }
                if (forced nonEmpty)
                    VobSub.buildNew(extractFile(f, t), extractIndex(f, t), forced) match {
                        case Right((_, idx)) => MkvToolNix.muxSub(stub(f, ".new.mkv"), f, idx)
                        case Left(oops)      => info("\t\t" + oops)
                    }

            case Left(oops) =>
                info("\t\t" + oops)
        }
    }

}
