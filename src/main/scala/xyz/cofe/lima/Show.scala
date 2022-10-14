package xyz.cofe.lima

import language.experimental.macros, magnolia1._

trait Show[T] {
  def show(t:T):String
}

object ShowDerivation {
  type Typeclass[T] = Show[T]

  def join[T](ctx: CaseClass[Show, T]): Show[T] = new Show[T] {
    def show(value: T): String = ctx.parameters.map { p =>
      s"${p.label}=${p.typeclass.show(p.dereference(value))}"
    }.mkString("{", ",", "}")
  }
  def split[T](ctx: SealedTrait[Show, T]): Show[T] =
    new Show[T] {
      def show(value: T): String = ctx.split(value) { sub =>
        sub.typeclass.show(sub.cast(value))
      }
    }

  implicit def gen[T]: Show[T] = macro Magnolia.gen[T]
}

object Show {
  implicit val intShow: Show[Int] = (t: Int) => t.toString
  implicit val longShow: Show[Long] = (t: Long) => t.toString
  implicit val doubleShow: Show[Double] = (t: Double) => t.toString
  implicit val stringShow: Show[String] = (t: String) => t.toString
  implicit val boolShow: Show[Boolean] = (t: Boolean) => t.toString
  implicit def optShow[T:Show]: Show[Option[T]] = {
    case Some(value) => implicitly[Show[T]].show(value)
    case None => "none"
  }
  implicit def mapShow[K:Show,V:Show]: Show[Map[K,V]] = (map:Map[K,V]) => {
    "{" + map.map{case(k,v)=> implicitly[Show[K]].show(k)+"="+implicitly[Show[V]].show(v)}.mkString(",") +
    "}"
  }
  implicit def listShow[V:Show]: Show[List[V]] = (list:List[V]) => {
    "[" + list.map(v=>implicitly[Show[V]].show(v)).mkString(",") + "]"
  }
}
