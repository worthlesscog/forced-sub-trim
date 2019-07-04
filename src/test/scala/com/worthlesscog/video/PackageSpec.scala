package com.worthlesscog.video

import java.io.File

import org.scalatest._

class PackageSpec extends FreeSpec with Matchers {

    "stub" - {
        "should strip the extension from a filename" in {
            stub(new File("this/is/my/file.jpg")) should be(new File("this/is/my/file"))
        }
        "should leave filenames without extensions unchanged" in {
            stub(new File("this/is/my/file")) should be(new File("this/is/my/file"))
        }
    }

}
