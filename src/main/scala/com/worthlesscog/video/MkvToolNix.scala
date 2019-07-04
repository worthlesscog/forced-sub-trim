package com.worthlesscog.video

import java.io.File

import spray.json._

case class TrackProps(
    default_track: Option[Boolean],
    enabled_track: Option[Boolean],
    forced_track: Option[Boolean],
    codec_id: Option[String],
    language: Option[String],
    pixel_dimensions: Option[String],
    default_duration: Option[Long],
    audio_channels: Option[Int],
    audio_sampling_frequency: Option[Int],
    track_name: Option[String]
)

case class Track(
    `type`: String,
    id: Int,
    codec: String,
    properties: Option[TrackProps]) {

    def ext = `type` substring(0, 3)
    def kind = `type` charAt 0

    def codecId = properties flatMap { _.codec_id }

    def description = {
        def opt(b: Option[Boolean], s: String) = if (b contains true) Some(s) else None

        def dumpProps(p: TrackProps) =
            Seq(p.language, p.codec_id,
                opt(p.enabled_track, "enabled"), opt(p.default_track, "default"), opt(p.forced_track, "forced"),
                p.pixel_dimensions,
                if (kind == 'v') frameRate map o2d else None,
                p.track_name, p.audio_channels, p.audio_sampling_frequency).flatten mkString ", "

        def dumpTrack = Seq(properties map dumpProps).flatten mkString ", "

        s"$kind${ i2d(id) } - ${ dumpTrack }"
    }

    def frameRate = properties flatMap { p =>
        p.default_duration map { d =>
            1.0 / (d / 1000000000d)
        }
    }

}

case class TrackList(
    tracks: List[Track]
)

object MkvToolNixProtocols extends DefaultJsonProtocol {
    implicit val fmtTrackProperties = jsonFormat10(TrackProps)
    implicit val fmtTrack = jsonFormat4(Track)
    implicit val fmtTrackList = jsonFormat1(TrackList)
}

object MkvToolNix {

    import MkvToolNixProtocols._

    val MKVEXTRACT = "mkvextract"
    val MKVMERGE = "mkvmerge"

    val CODEC_VOBSUB = "VobSub"

    val EXT_UNKNOWN = "unk"

    val TYPE_AUDIO = "audio"
    val TYPE_SUBTITLE = "subtitles"
    val TYPE_VIDEO = "video"

    val VOBSUB_INDEX = ".idx"
    val VOBSUB_SUBTITLES = ".sub"

    def demux(f: File, ts: List[Track]) = {
        val missing = missingExtracts(f, ts).map(trackDescriptor(f))
        val command = Seq(MKVEXTRACT, "tracks", f toString) ++ missing
        exec(command: _*)
    }

    def missingExtracts(f: File, ts: List[Track]) =
        ts filterNot { t =>
            val extract = new File(extractedTrackName(f, t))
            if (t.codecId contains CODEC_VOBSUB)
                extract.exists && new File(extractedTrackIndex(f, t)).exists
            else
                extract.exists
        }

    def extractedTrackName(f: File, t: Track) =
        extractedTrackStub(f, t) + "." + extension(t).getOrElse(EXT_UNKNOWN)

    def extractedTrackStub(f: File, t: Track) =
        "" + stub(f, "." + i2d(t id))

    def extension(t: Track) = {
        t.properties flatMap { p =>
            p.codec_id map {
                case "A_AC3"       => "ac3"
                case "A_DTS"       => "dts"
                case "A_PCM"       => "wav"
                case "S_TEXT/UTF8" => "srt"
                case "S_VOBSUB"    => "sub"
                case "V_MPEG2"     => "mpg"
                case _             => EXT_UNKNOWN
            }
        }
    }

    def extractedTrackIndex(f: File, t: Track) =
        extractedTrackStub(f, t) + VOBSUB_INDEX

    def trackDescriptor(f: File)(t: Track) =
        "" + t.id + ":" + extractedTrackName(f, t)

    def extractChapters(implicit f: File) =
        exec(MKVEXTRACT, f.toString, "chapters", stub(f, ".ch.xml").toString)

    def muxSub(newMkv: File, oldMkv: File, subIdx: File) =
        exec(
            MKVMERGE,
            "--output", newMkv.toString,
            "--no-subtitles",
            "(", oldMkv.toString, ")",
            "--forced-track", "0:yes",
            "(", subIdx.toString, ")"
        )

    // XXX - this could fall on it's face
    def trackList(f: File) =
        execResult(MKVMERGE, "-J", f toString).parseJson.convertTo[TrackList].tracks

}
