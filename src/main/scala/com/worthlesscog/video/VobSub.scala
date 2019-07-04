package com.worthlesscog.video

import java.io.{BufferedInputStream, BufferedWriter, File, FileOutputStream, RandomAccessFile}

import scala.io.{BufferedSource, Source}

class VobSubPacket(bs: Array[Byte]) {

    // see https://en.wikipedia.org/wiki/MPEG_program_stream
    //     https://en.wikipedia.org/wiki/Packetized_elementary_stream
    //     https://forums.xilinx.com/xlnx/attachments/xlnx/DSPTOOL/15095/1/iso13818-1.pdf
    //     http://dvd.sourceforge.net/dvdinfo/packhdr.html
    //     http://dvd.sourceforge.net/dvdinfo/pes-hdr.html

    // MPEG-2 PS pack header, 14 bytes + stuffing
    lazy val psSyncBytes = bs.slice(0, 4)
    lazy val psMpegTwoMarker = bs(4) >> 6 & 0x03
    lazy val psMarkerBits = 0 |
        (bs(4) & 0x04) << 3 |
        (bs(6) & 0x04) << 2 |
        (bs(8) & 0x04) << 1 |
        (bs(9) & 0x01) << 2 |
        (bs(12) & 0x03)
    //    lazy val psScr = 0L |
    //        (bs(4) & 0x38).toLong << 27 |
    //        (bs(4) & 0x03) << 28 |
    //        (bs(5) & 0xFF) << 20 |
    //        (bs(6) & 0xF8) << 12 |
    //        (bs(6) & 0x03) << 13 |
    //        (bs(7) & 0xFF) << 5 |
    //        (bs(8) & 0xF8) >> 3
    //    lazy val psScrExtension = 0 |
    //        (bs(8) & 0x03) << 7 |
    //        (bs(9) & 0xFE) >> 1
    lazy val psBitRate = 0 |
        (bs(10) & 0xFF) << 14 |
        (bs(11) & 0xFF) << 6 |
        (bs(12) & 0xFC) >> 2
    lazy val psStuffingLength = bs(13) & 0x07

    def psHeaderValid =
        psSyncBytes.sameElements(VobSub.PS_PACK_HEADER_SYNC_BYTES) &&
            psMpegTwoMarker == 0x01 &&
            psMarkerBits == 0x3F &&
            psBitRate > 0

    // MPEG-2 PES packet header, stream types other than Padding/Private Stream 2
    lazy val pesPacketHeaderOffset = psStuffingLength + 14
    lazy val pesPacketStartCodePrefix = bs.slice(pesPacketHeaderOffset, pesPacketHeaderOffset + 3)
    lazy val pesStreamId = bs(pesPacketHeaderOffset + 3)
    lazy val pesPacketLength = word(bs, pesPacketHeaderOffset + 4)
    lazy val pesOneOh = bs(pesPacketHeaderOffset + 6) >> 6 & 0x03
    //    lazy val pesScramblingControl = bs(pesPacketHeaderOffset + 6) >> 4 & 0x03
    //    lazy val pesPriority = flag(bs(pesPacketHeaderOffset + 6) >> 3)
    //    lazy val pesDataAlignmentIndicator = flag(bs(pesPacketHeaderOffset + 6) >> 2)
    //    lazy val pesCopyright = flag(bs(pesPacketHeaderOffset + 6) >> 1)
    //    lazy val pesOriginalOrCopy = flag(bs(pesPacketHeaderOffset + 6))
    lazy val pesPtsDtsFlags = bs(pesPacketHeaderOffset + 7) >> 6 & 0x03
    //    lazy val pesEscrFlag = flag(bs(pesPacketHeaderOffset + 7) >> 5)
    //    lazy val pesEsRateFlag = flag(bs(pesPacketHeaderOffset + 7) >> 4)
    //    lazy val pesDsmTrickModeFlag = flag(bs(pesPacketHeaderOffset + 7) >> 3)
    //    lazy val pesAdditionalCopyInfoFlag = flag(bs(pesPacketHeaderOffset + 7) >> 2)
    //    lazy val pesCrcFlag = flag(bs(pesPacketHeaderOffset + 7) >> 1)
    //    lazy val pesExtensionFlag = flag(bs(pesPacketHeaderOffset + 7))
    lazy val pesHeaderDataLength = bs(pesPacketHeaderOffset + 8) & 0xFF
    lazy val pesOptionalFields = bs.slice(pesPacketHeaderOffset + 9, pesPacketHeaderOffset + 9 + pesHeaderDataLength)
    //    lazy val pesPtsOnlyMarker = pesOptionalFields(0) >> 4 & 0x0F
    lazy val pesPts = 0L |
        (pesOptionalFields(0) & 0x0E).toLong << 29 |
        (pesOptionalFields(1) & 0xFF) << 22 |
        (pesOptionalFields(2) & 0xFE) << 14 |
        (pesOptionalFields(3) & 0xFF) << 7 |
        (pesOptionalFields(4) & 0xFE) >> 1
    //    lazy val pesPtsMarkerBits = 0 |
    //        (pesOptionalFields(0) & 0x01) << 2 |
    //        (pesOptionalFields(2) & 0x01) << 1 |
    //        (pesOptionalFields(4) & 0x01)

    def pesHeaderValid =
        pesPacketStartCodePrefix.sameElements(VobSub.PES_PACKET_START_CODE_PREFIX) &&
            pesOneOh == 0x02 &&
            pesPtsDtsFlags != 0x01

    def isFirstSPU = pesPtsDtsFlags == 0x02
    def isPrivateStream1 = pesStreamId == VobSub.PES_PRIVATE_STREAM_1

    lazy val pesPayloadOffset = pesPacketHeaderOffset + 9 + pesHeaderDataLength
    lazy val data = bs.slice(pesPayloadOffset, pesPayloadOffset + (pesPacketLength - (pesHeaderDataLength + 3)))

}

case class SubIndex(timestamp: String, offset: Long)

case class DisplayControlSequence(delay: Int, commands: Seq[Byte])

class SubPictureUnit(bs: Array[Byte], pts: Long, streamId: Byte, val offset: Int, val packets: Int) {

    // see http://dvd.sourceforge.net/dvdinfo/spu.html
    //     http://sam.zoy.org/writings/dvd/subtitles/

    import VobSub._

    lazy val packetSize = word(bs, 0)
    lazy val controlSequenceTableOffset = word(bs, 2)
    lazy val pixelData = bs.slice(4, controlSequenceTableOffset)
    lazy val subPictureDisplayControlSequenceTable = bs.slice(controlSequenceTableOffset, packetSize)
    lazy val subPictureDisplaySequences = parseDisplayControlSequenceTable()

    lazy val isForced = subPictureDisplaySequences exists { _.commands contains CMD_FSTA_DSP }
    lazy val timeStamp = secsToHrsMinsSecs(pts / 90000d)

    def isIncomplete = packetSize != bs.length

    def parseDisplayControlSequenceTable() = {

        def parseSequence(bs: Seq[Byte], cmds: List[Byte] = Nil): List[Byte] =
            bs match {
                // case Seq(CMD_CHG_COLCON, t@_*)                  => ???
                case Seq()                                      => ???
                case Seq(CMD_FSTA_DSP, t@_*)                    => parseSequence(t, CMD_FSTA_DSP :: cmds)
                case Seq(CMD_STA_DSP, t@_*)                     => parseSequence(t, CMD_STA_DSP :: cmds)
                case Seq(CMD_STP_DSP, t@_*)                     => parseSequence(t, CMD_STP_DSP :: cmds)
                case Seq(CMD_SET_COLOR, _, _, t@_*)             => parseSequence(t, CMD_SET_COLOR :: cmds)
                case Seq(CMD_SET_CONTR, _, _, t@_*)             => parseSequence(t, CMD_SET_CONTR :: cmds)
                case Seq(CMD_SET_DAREA, _, _, _, _, _, _, t@_*) => parseSequence(t, CMD_SET_DAREA :: cmds)
                case Seq(CMD_SET_DSPXA, _, _, _, _, t@_*)       => parseSequence(t, CMD_SET_DSPXA :: cmds)
                case Seq(CMD_END, _*)                           => (CMD_END :: cmds) reverse
                case _                                          => ???
            }

        def parseSequences(ix: Int, seqs: List[DisplayControlSequence]): List[DisplayControlSequence] = {
            val delay = word(subPictureDisplayControlSequenceTable, ix)
            val next = word(subPictureDisplayControlSequenceTable, ix + 2)
            val cmds = parseSequence(subPictureDisplayControlSequenceTable.slice(ix + 4, subPictureDisplayControlSequenceTable.length))

            val seq = DisplayControlSequence(delay, cmds)
            if (next != controlSequenceTableOffset + ix)
                parseSequences(next - controlSequenceTableOffset, seq :: seqs)
            else
                (seq :: seqs) reverse
        }

        parseSequences(0, Nil)

    }

}

class SubtitleStream(stream: BufferedInputStream) {

    var filepos = 0
    var offset = 0

    def next: Maybe[Option[SubPictureUnit]] = {
        filepos = offset
        nextBlock match {
            case Right(Array()) =>
                Right(None)

            case Right(bs) =>
                val p = new VobSubPacket(bs)
                if (p.psHeaderValid && p.pesHeaderValid && p.isPrivateStream1 && p.isFirstSPU)
                    buildSPU(p) match {
                        case Right(spu) => Right(Some(spu))
                        case Left(oops) => Left(oops)
                    }
                else
                    next

            case Left(oops) =>
                Left(oops)
        }
    }

    private def nextBlock = {
        offset += VobSub.BLOCK_SIZE
        readBytes(VobSub.BLOCK_SIZE)(stream)
    }

    private def buildSPU(p: VobSubPacket): Maybe[SubPictureUnit] = {

        def append(l: List[Array[Byte]], rem: Int, pkts: Int): Maybe[(List[Array[Byte]], Int)] =
            nextBlock match {
                case Right(Array()) =>
                    Left(INCOMPLETE_READ)

                case Right(bs) =>
                    val t = new VobSubPacket(bs).data.tail
                    if (rem > t.length)
                        append(t :: l, rem - t.length, pkts + 1)
                    else
                        Right(t.slice(0, rem) :: l, pkts + 1)

                case Left(oops) =>
                    Left(oops)
            }

        val (streamId, data) = (p.data(0), p.data.tail)
        val spu = new SubPictureUnit(data, p.pesPts, streamId, filepos, 1)
        if (spu isIncomplete)
            append(data :: Nil, spu.packetSize - data.length, 1) match {
                case Right((l, pkts)) => Right(new SubPictureUnit(l.reverse.flatten.toArray, p.pesPts, streamId, filepos, pkts))
                case Left(oops)       => Left(oops)
            }
        else
            Right(spu)

    }

}

object VobSub {

    val BLOCK_SIZE = 2048

    val CMD_FSTA_DSP: Byte = 0
    val CMD_STA_DSP: Byte = 1
    val CMD_STP_DSP: Byte = 2
    val CMD_SET_COLOR: Byte = 3
    val CMD_SET_CONTR: Byte = 4
    val CMD_SET_DAREA: Byte = 5
    val CMD_SET_DSPXA: Byte = 6
    val CMD_CHG_COLCON: Byte = 7
    val CMD_END: Byte = -1

    val PS_PACK_HEADER_SYNC_BYTES = Array[Byte](0x00, 0x00, 0x01, 0xBA.toByte)

    val PES_PACKET_START_CODE_PREFIX = Array[Byte](0x00, 0x00, 0x01)
    val PES_PRIVATE_STREAM_1 = 0xBD.toByte

    val NEW_PREFIX = "new-"

    val TIMESTAMP = """^timestamp: ((?:\d\d:){3})(\d{3}), filepos: (.{9})$""".r

    def open(f: File): SubtitleStream =
        new SubtitleStream(bis(fis(f)))

    def buildNew(oldSub: File, oldIdx: File, spus: List[SubPictureUnit]): Maybe[(File, File)] = {
        val newIdx = File.createTempFile(NEW_PREFIX, suffix(oldIdx), oldIdx.getParentFile)
        buildNewIndex(oldIdx, newIdx, spus)

        val newSub = stub(newIdx, suffix(oldSub))
        buildNewSub(oldSub, newSub, spus)

        Right((newSub, newIdx))
    }

    def buildNewIndex(oldIdx: File, newIdx: File, spus: List[SubPictureUnit]) =
        using(Source.fromFile(oldIdx)) { o =>
            using(bw(osw(fos(newIdx)))) { n =>
                copyHead(o, n)
                dumpTimestamps(n, spus)
            }
        }

    def copyHead(o: BufferedSource, n: BufferedWriter) = {
        def copy(i: Iterator[String]): Unit =
            if (i.hasNext)
                i.next match {
                    case TIMESTAMP(_, _, _) =>
                    case s                  => n.write(s + "\n"); copy(i)
                }

        copy(o.getLines)
    }

    def dumpTimestamps(n: BufferedWriter, spus: List[SubPictureUnit]) = {
        def dump(spus: List[SubPictureUnit], offset: Long): Unit = spus match {
            case h :: t =>
                n.write(f"timestamp: ${ h.timeStamp }, filepos: $offset%09x\n")
                dump(t, offset + h.packets * BLOCK_SIZE)

            case _ =>
        }

        dump(spus, 0)
    }

    def buildNewSub(oldSub: File, newSub: File, spus: List[SubPictureUnit]) =
        using(raf(oldSub)) { o =>
            using(fos(newSub)) { n =>
                copySubs(o, n, spus)
            }
        }

    def copySubs(o: RandomAccessFile, n: FileOutputStream, spus: List[SubPictureUnit]) = {
        def copy(spus: List[SubPictureUnit]): Option[String] = spus match {
            case h :: t =>
                val bytes = BLOCK_SIZE * h.packets
                val buff = new Array[Byte](bytes)
                o.seek(h.offset)
                o.read(buff) match {
                    case count if count == bytes => n.write(buff); copy(t)
                    case _                       => Some(INCOMPLETE_READ)
                }

            case _ => None
        }

        copy(spus)
    }

}
