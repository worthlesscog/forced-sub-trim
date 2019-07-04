package com.worthlesscog

import java.io._

import scala.sys.process._

package object video {

    type Maybe[T] = Either[String, T]

    // implicit def implicitBytes(i: Int) = i.toByte

    implicit class FilePimp(f: File) {
        def dirs = f.listFiles { _.isDirectory }
        def files = f.listFiles { _.isFile }
        def sorted = f.listFiles.sorted
        def sortedDirs = dirs.sorted
        def sortedFiles = files.sorted
    }

    implicit class Pipe[A](a: A) {
        def |>[B](f: A => B): B = f(a)
    }

    val INCOMPLETE_READ = "Incomplete read"

    val sink = (_: String) => {}
    val capture = ProcessLogger(sink, sink)

    def bis(s: InputStream) = new BufferedInputStream(s)
    def bw(w: Writer) = new BufferedWriter(w)
    def fis(implicit f: File) = new FileInputStream(f)
    def fos(implicit f: File) = new FileOutputStream(f)
    def osw(s: OutputStream) = new OutputStreamWriter(s)
    def raf(f: File) = new RandomAccessFile(f, "r")

    def exec(command: String*) =
        osSpecificCall(command) ! capture match {
            case 0 => Right(0)
            case x => Left(x)
        }

    def execResult(command: String*) =
        osSpecificCall(command) !!

    def flag(b: Int) =
        (b & 0x01) == 0x01

    def i2d(i: Int) =
        f"$i%02d"

    def info(s: String) =
        println(s)

    def o2d(d: Double) =
        f"$d%.02f"

    def osIsWindows() =
        sys.props("os.name").toLowerCase contains "windows"

    def osSpecificCall(command: Seq[String]) =
        if (osIsWindows())
            Seq("cmd", "/C") ++ command
        else
            command

    def readBytes(count: Int)(implicit s: InputStream): Maybe[Array[Byte]] =
        try {
            val bs = Array.ofDim[Byte](count)
            s.read(bs) match {
                case n if n == count => Right(bs)
                case -1              => Right(Array())
                case _               => Left(INCOMPLETE_READ)
            }
        } catch {
            case x: IOException => Left(x getMessage)
        }

    def secsToHrsMinsSecs(secs: Double) = {
        val h = (secs / 3600) toInt
        val m = ((secs % 3600) / 60) toInt
        val s = (secs % 3600) % 60

        f"$h%02d.$m%02d.$s%06.3f" replaceAll("\\.", ":")
    }

    def stub(f: File, newExt: String = "") = {
        val n = f.getName
        val ix = n.lastIndexOf('.')
        if (ix >= 0)
            new File(f getParent, n.substring(0, ix) + newExt)
        else
            new File(f getParent, n + newExt)
    }

    def suffix(f: File) = {
        val n = f.getName
        n.lastIndexOf('.') match {
            case -1 => ""
            case i  => n.substring(i)
        }
    }

    def using[A <: {def close() : Unit}, B](closeable: A)(f: A => B): B =
        try f(closeable) finally closeable.close()

    def walk(dir: File, v: File => Unit): Unit = {
        def w(fs: List[File]): Unit = fs match {
            case d :: t if d isDirectory => w(t ++ d.sorted)
            case f :: t                  => v(f); w(t)
            case _                       =>
        }

        w(dir.sorted toList)
    }

    def word(bs: Array[Byte], offset: Int) =
        (bs(offset) & 0xFF) << 8 | (bs(offset + 1) & 0xFF)

}
