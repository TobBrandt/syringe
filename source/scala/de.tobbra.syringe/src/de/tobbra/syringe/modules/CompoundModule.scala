package de.tobbra.syringe.modules

import de.tobbra.syringe.Context

class CompoundModule[-C <: Context](val name: String, val subModules: Module[C]*) extends Module[C] {
    def bindings = subModules.flatMap(_.bindings)
}