package de.tobbra.syringe

trait BasicContext extends Context {
    val singletonScope: Scope
    val threadLocalScope: Scope
    val transientScope: Scope
}