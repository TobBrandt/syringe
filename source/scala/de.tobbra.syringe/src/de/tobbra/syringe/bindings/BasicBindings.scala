package de.tobbra.syringe.bindings

import de.tobbra.syringe._
import scala.reflect.runtime.universe.{ TypeTag, Type }

trait BasicBindings {
    type Ctx <: Context
    def bindInstance[T: TypeTag](instance: T)
    def bindInstance(t: Type, instance: Any)
    def bindProvider[T: TypeTag](provider: Ctx => T, scope: Ctx => Scope)
    def bindProvider(t: Type, provider: Ctx => Any, scope: Ctx => Scope)

    // convenience method
    def bindProvider[T: TypeTag](provider: => T, scope: => Scope) {
        bindProvider((ctx: Ctx) => provider, (ctx: Ctx) => scope)
    }
}