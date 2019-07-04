package com.worthlesscog.video

import org.scalatest._

class VobSubSpec extends FreeSpec with Matchers {

    implicit def implicitBytes(i: Int) = i.toByte

    def hdr(bs: Array[Byte]) =
        new VobSubPacket(bs)

    val emptyHeader = hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00))

    "psSyncBytes" - {
        "should return the pack header bytes" in {
            //        0     1     2     3     4     5     6     7     8     9    10    11    12    13
            hdr(Array(0x04, 0x03, 0x02, 0x01, 0x00)).psSyncBytes should be(Array(4, 3, 2, 1))
            hdr(Array(0x00, 0x01, 0x02, 0x03, 0x04)).psSyncBytes should be(Array(0, 1, 2, 3))
        }
    }

    "psMpegTwoMarker" - {
        "should return the MPEG-2 marker bits" in {
            //        0     1     2     3     4     5     6     7     8     9    10    11    12    13
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x40)).psMpegTwoMarker should be(1)
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x80)).psMpegTwoMarker should be(2)
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0xC0)).psMpegTwoMarker should be(3)
            emptyHeader.psMpegTwoMarker should be(0)
        }
    }

    "psMarkerBits" - {
        "should return the always-on marker bits" in {
            //        0     1     2     3     4     5     6     7     8     9     10    11    12    13
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x04, 0x00, 0x04, 0x01, 0x00, 0x00, 0x03)).psMarkerBits should be(63)
            emptyHeader.psMarkerBits should be(0)
        }
    }

    //    "psScr" - {
    //        "should return the complete system clock reference" in {
    //            //        0     1     2     3     4     5     6     7     8     9    10    11    12    13
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x38, 0x00, 0x00, 0x00, 0x00)).psScr should be(0x00000001C0000000L)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00)).psScr should be(0x0000000030000000L)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00, 0x00)).psScr should be(0x000000000FF00000L)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF8, 0x00, 0x00)).psScr should be(0x00000000000F8000L)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00)).psScr should be(0x0000000000006000L)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00)).psScr should be(0x0000000000001FE0L)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xF8)).psScr should be(0x000000000000001FL)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x3B, 0xFF, 0xFB, 0xFF, 0xF8)).psScr should be(0x00000001FFFFFFFFL)
    //            emptyHeader.psScr should be(0L)
    //        }
    //    }

    //    "psScrExtension" - {
    //        "should return the complete system clock reference extension" in {
    //            //        0     1     2     3     4     5     6     7     8     9    10    11    12    13
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00)).psScrExtension should be(0x00000180)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFE)).psScrExtension should be(0x0000007F)
    //            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0xFE)).psScrExtension should be(0x000001FF)
    //            emptyHeader.psScrExtension should be(0)
    //        }
    //    }

    "psBitRate" - {
        "should return the complete bit rate" in {
            //        0     1     2     3     4     5     6     7     8     9     10    11    12    13
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00, 0x00)).psBitRate should be(0x003FC000)
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0x00)).psBitRate should be(0x00003FC0)
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFC)).psBitRate should be(0x0000003F)
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF, 0xFC)).psBitRate should be(0x003FFFFF)
            emptyHeader.psBitRate should be(0)
        }
    }

    "psStuffingLength" - {
        "should return the stuffing length" in {
            //        0     1     2     3     4     5     6     7     8     9     10    11    12    13
            hdr(Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07)).psStuffingLength should be(7)
            emptyHeader.psStuffingLength should be(0)
        }
    }

    "psHeaderValid" - {
        "should be true for valid headers" in {
            //        0     1     2     3     4     5     6     7     8     9     10    11    12    13
            hdr(Array(0x00, 0x00, 0x01, 0xBA, 0x44, 0x00, 0x04, 0x00, 0x04, 0x01, 0x00, 0x01, 0x03, 0x00)).psHeaderValid should be(true)
            emptyHeader.psHeaderValid should be(false)
        }
    }

}
