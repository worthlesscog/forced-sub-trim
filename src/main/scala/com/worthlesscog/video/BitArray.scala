package com.worthlesscog.video

class BitArray(bs: Array[Byte]) {

    // XXX - fails with "large" (MAXINT / 8 ish) arrays due to counter overflow
    private var bit = 0

    def flag() =
        take(1) == 1

    def reset() =
        bit = 0

    // XXX - simplistic, slow, no foot protection
    //       should be 3 steps - byte boundary, bytes, trailing bits
    def take(bits: Int) = {
        // var i = 0
        // for (_ <- 0 until bits) {
        //     i = (i << 1) | bs(bit >> 3) >> (bit & 7 ^ 7) & 1
        //     bit += 1
        // }
        // i

        val i = (bit until bit + bits).foldLeft(0) { (a, b) => (a << 1) | bs(b >> 3) >> (b & 7 ^ 7) & 1 }
        bit += bits
        i
    }

    def takeAsBytes(bits: Int) = {
        val q = for (_ <- 0 until bits >> 3) yield take(8).toByte
        val r = if ((bits & 7) > 0) Array((take(bits & 7) << 8 - (bits & 7)).toByte) else Array.empty[Byte]
        val b = q.toBuffer
        b.appendAll(r)
        b.toArray
    }

}
