package com.sky.jloc

import java.io.File

import scala.io.Source

object FileCounterApp extends App {

  def countFiles(path: String) = fileList(path).size

  def countLines(path: String) = fileDetails(path, "*", line => true).lines

  def fileDetails(path: String, extension: String = "java", commentFilter: String => Boolean) = {
    val files = filesByExtension(path, extension)
    val fileCount = files.size
    val lines = files.flatMap(Source.fromFile(_).getLines).map(_.trim)
    val blank = lines.filter(_.trim.length == 0).size
    val comment = lines.filter(commentFilter).size
    val code = lines.size - blank - comment

    FileDetails(fileCount, blank, comment, code)
  }

  def filesByExtension(path: String, extension: String) = {
    if (extension == "*") fileList(path)
    else fileList(path).filter(_.getName.endsWith(s".$extension"))
  }

  def fileList(path: String): List[File] = {
    if (path == null) throw new IllegalArgumentException("path must not be null or empty!")
    val file = new File(path)
    if (!file.exists()) throw new IllegalArgumentException("path doesn't exists!")
    filesStream(file).toList
  }

  private def filesStream(file: File): Stream[File] = if (file.isDirectory) file.listFiles().toStream.flatMap(filesStream) else Stream.cons(file, Stream.empty)

  case class FileDetails(files: Int, blank: Int, comment: Int, code: Int) {
    val lines = blank + comment + code
  }

  println(countFiles(args(0)))
  println(countLines(args(0)))
  println(fileDetails(args(0), "java", line => line.startsWith("/*") || line.startsWith("*/") || line.startsWith("*")))
}