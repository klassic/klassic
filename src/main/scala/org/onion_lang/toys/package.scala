package org.onion_lang

import scala.language.reflectiveCalls
import java.io.{FileInputStream, InputStreamReader, BufferedReader}

/**
 * @author Kota Mizushima
 */
package object toys {
  def openReader[A](fileName: String)(f: BufferedReader => A): A = {
    val reader = new BufferedReader(new InputStreamReader(new FileInputStream(fileName), "UTF-8"))
    using(reader)(f)
  }
  def using[A <: { def close(): Unit}, B](resource: A)(f: A => B): B = try {
    f(resource)
  } finally {
    scala.util.control.Exception.allCatch(resource.close())
  }
}
