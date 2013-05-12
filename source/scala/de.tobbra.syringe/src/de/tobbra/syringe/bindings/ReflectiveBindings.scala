package de.tobbra.syringe.bindings

import de.tobbra.syringe._
import scala.reflect.runtime.{ universe => ru }
import ru.{ TypeTag, typeOf, MethodSymbol, Type }

trait ReflectiveBindings extends BasicBindings {
    type Ctx <: BasicContext
    private val rm = ru.runtimeMirror(getClass.getClassLoader)

    def bindTransient[P: TypeTag, B: TypeTag] = bindInScope[P, B](_.transientScope)
    def bindSingleton[P: TypeTag, B: TypeTag] = bindInScope[P, B](_.singletonScope)
    def bindThreadLocal[P: TypeTag, B: TypeTag] = bindInScope[P, B](_.threadLocalScope)
    def bindInScope[P: TypeTag, B: TypeTag](scope: Ctx => Scope) = bindProvider(makeProvider[P, B], scope)

    def bindTransient(providedType: Type, boundType: Type) = bindInScope(providedType, boundType, _.transientScope)
    def bindSingleton(providedType: Type, boundType: Type) = bindInScope(providedType, boundType, _.singletonScope)
    def bindThreadLocal(providedType: Type, boundType: Type) = bindInScope(providedType, boundType, _.threadLocalScope)

    def bindInScope(providedType: Type, boundType: Type, scope: Ctx => Scope) {
        require(boundType <:< providedType, s"Bound type $boundType must be subtype of provided type $providedType.")
        bindProvider(providedType, makeProvider(providedType, boundType), scope)
    }

    private def makeProvider[P: TypeTag, B: TypeTag]: Ctx => P = {
        val anyP = makeProvider(typeOf[P], typeOf[B])
        ctx => anyP(ctx).asInstanceOf[P]
    }

    private def makeProvider(p: Type, b: Type): Ctx => Any = {
        require(b <:< p, s"Bound type $b must be subtype of provided type $p.")
        val tsym = b.typeSymbol
        require(tsym.isClass, s"Bound type $b (providing $p) must be a class.")
        val ctorSym = b.declaration(ru.nme.CONSTRUCTOR).asTerm.alternatives.collectFirst {
            case s: MethodSymbol if s.isPrimaryConstructor => s
        }
        require(!ctorSym.isEmpty, s"Bound type $b must have a primary constructor.")
        val paramTypes = ctorSym.get.paramss(0).map(_.typeSignature)
        val ctor = rm.reflectClass(tsym.asClass).reflectConstructor(ctorSym.get)
        (ctx: Ctx) => ctor(paramTypes.map(ctx.factory.get(_)): _*)
    }
}