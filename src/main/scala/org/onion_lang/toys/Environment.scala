package org.onion_lang
package toys
import scala.collection.mutable

/**
 * @author Kota Mizushima
 */
class Environment(val parent:Option[Environment]) {
  val variables = mutable.Map[String, Value]()
  def apply(key: String): Value = {
    variables.get(key).getOrElse {
      parent.map(_.apply(key)).getOrElse {
        throw new Exception("symbol'%s' not found".format(key))
      }
    }
  }
  def set(key: String, value: Value): Value = {
    def iset(optEnv: Option[Environment]): Unit = optEnv match {
      case Some(env) => if(env.variables.contains(key)) env(key) = value else iset(env.parent)
      case None => ()
    }
    iset(Some(this))
    value
  }
  def update(key: String, value: Value): Value = {
    variables(key) = value
    value
  }
}
