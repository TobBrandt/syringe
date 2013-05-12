package de.tobbra.syringe

import scala.reflect.runtime.universe._
import de.tobbra.syringe.modules.Module

trait Factory {

    final def get[T: TypeTag]: T = get(typeOf[T]).asInstanceOf[T]
    final def get(t: Type): Any = provide(t).apply()
    final def provide[T: TypeTag]: () => T = provide(typeOf[T]).asInstanceOf[() => T]
    def provide(t: Type): () => Any
}

object Factory {
    def apply(modules: Module[BasicContext]*) =
        new DefaultFactory(modules: _*) with BasicContextProvider
}