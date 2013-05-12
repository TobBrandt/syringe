package de.tobbra.syringe.modules

import de.tobbra.syringe.bindings._
import de.tobbra.syringe._

import scala.reflect.runtime.universe.{ TypeTag, Type, typeOf, runtimeMirror }

trait BasicModule[C <: BasicContext] extends Module[C] with BasicBindings {

    private var _bindings = List[Binding[Ctx]]()
    private val rm = runtimeMirror(getClass.getClassLoader)

    def bindings = _bindings

    def addBinding(binding: Binding[Ctx]) = _bindings = binding :: _bindings

    def bindInstance[T: TypeTag](instance: T) {
        bindInstanceInternal(typeOf[T], instance)
    }

    def bindInstance(t: Type, instance: Any) {
        require(rm.runtimeClass(t).isAssignableFrom(instance.getClass),
            s"Bound instance of class ${instance.getClass} must be assignable to type $t")
        bindInstanceInternal(t, instance)
    }

    private def bindInstanceInternal(t: Type, instance: Any) {
        addBinding(InstanceBinding(t, instance))
    }

    def bindProvider[T: TypeTag](provider: Ctx => T, scope: Ctx => Scope) {
        bindProvider(typeOf[T], provider, scope)
    }

    def bindProvider(t: Type, provider: Ctx => Any, scope: Ctx => Scope) {
        addBinding(ScopedBinding(t, provider, scope))
    }
}