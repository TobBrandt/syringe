package de.tobbra.syringe.modules

import de.tobbra.syringe.bindings.Binding
import de.tobbra.syringe.Context
import de.tobbra.syringe.BasicContext

trait Module[-C <: Context] {
    type Ctx = C
    def bindings: Seq[Binding[Ctx]]
    def name: String
}

object Module {
    type Basic = BasicModule[BasicContext]
}