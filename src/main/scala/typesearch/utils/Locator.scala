/*
 * Copyright Thibault Duplessis
 * https://github.com/ornicar/scalex/blob/master/core/src/main/scala/scalex/dump/Locator.scala
 * Dec 28 2011
 */

package typesearch.utils

import java.io.File

class Locator {

  var regex = """^(.+).scala$""".r

  def locate(locations: List[File]): List[File] = {
    val (dirs, files) = locations span (_.isDirectory)

    walkDirs(dirs) ::: files
  }

  // Transforms a list of dirs to a list of scala files
  private[this] def walkDirs(dirs: List[File]): List[File] = {
    dirs flatMap { dir =>
      dir.listFiles.toList flatMap { file =>
        if (file.isDirectory) walkDirs(List(file))
        else {
          if (file.getName endsWith ".scala") List(file)
          else Nil
        }
      }
    }
  }
}