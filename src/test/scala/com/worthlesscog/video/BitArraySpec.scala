package com.worthlesscog.video

import org.scalatest._

class BitArraySpec extends FreeSpec with Matchers {

    implicit def implicitBytes(i: Int) = i.toByte

    // XXX - 6E5C => 0110 1110 0101 1100
    val subj = new BitArray(Array(0x6E, 0x5C))

    "flag" - {
        "should return true for 1 bits" in {
            subj.reset()
            subj.flag() should be(false)
            subj.flag() should be(true)
            subj.flag() should be(true)
            subj.flag() should be(false)
            subj.flag() should be(true)
            subj.flag() should be(true)
            subj.flag() should be(true)
            subj.flag() should be(false)
        }
    }

    "take" - {
        "should extract appropriate numbers of bits" in {
            subj.reset()
            subj.take(1) should be(0)
            subj.take(1) should be(1)
            subj.take(2) should be(2)
            subj.take(2) should be(3)
            subj.take(3) should be(4)
            subj.take(3) should be(5)
            subj.take(3) should be(6)
            subj.take(1) should be(0)
        }
    }

    "takeAsBytes" - {
        "should extract return extracted bits as byte arrays" in {
            // XXX - 6E5C => 0110 1110 0101 1100
            subj.reset()
            subj.takeAsBytes(1) should be(Array[Byte](0))
            subj.takeAsBytes(2) should be(Array[Byte](0xC0))
            subj.takeAsBytes(4) should be(Array[Byte](0x70))
            subj.takeAsBytes(8) should be(Array[Byte](0x2E))

            subj.reset()
            subj.takeAsBytes(16) should be(Array[Byte](0x6E, 0x5C))
        }
    }

    "reset" - {
        "should reset the bit cursor" in {
            subj.reset()
            subj.take(16) should be(28252)
            subj.reset()
            subj.take(16) should be(28252)
        }
    }

}
