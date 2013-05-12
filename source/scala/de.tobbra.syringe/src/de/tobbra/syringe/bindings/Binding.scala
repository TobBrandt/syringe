package de.tobbra.syringe.bindings

import scala.reflect.runtime.universe.{ TypeTag, typeOf, Type }
import de.tobbra.syringe._

sealed abstract class Binding[-C <: Context] {
    def provider: C => Any
    def scope: C => Scope
    val providedType: Type
}

case class InstanceBinding[-C <: Context](val providedType: Type, val instance: Any) extends Binding[C] {
    val provider = (ctx: C) => instance
    val scope = (ctx: C) => TransientScope
}

case class ScopedBinding[-C <: Context](val providedType: Type, val provider: C => Any, val scope: C => Scope) extends Binding[C]