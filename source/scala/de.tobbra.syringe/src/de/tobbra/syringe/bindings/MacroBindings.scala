package de.tobbra.syringe.bindings

import de.tobbra.syringe._
import scala.reflect.macros

trait MacroBindings extends BasicBindings {
    import scala.language.experimental.macros
    import de.tobbra.syringe.macros.MacroBindingsHelper

    type Ctx <: BasicContext

    def bindTransient[P, B] = macro MacroBindingsHelper.bindTransient[P, B, BasicContext, Scope]
    def bindSingleton[P, B] = macro MacroBindingsHelper.bindSingleton[P, B, BasicContext, Scope]
    def bindThreadLocal[P, B] = macro MacroBindingsHelper.bindThreadLocal[P, B, BasicContext, Scope]
    def bindInScope[P, B](scope: Ctx => Scope) = macro MacroBindingsHelper.bindInScope[P, B, BasicContext, Scope]

    def makeProvider[P, B]: Ctx => P = macro MacroBindingsHelper.makeProvider[P, B, BasicContext]
}