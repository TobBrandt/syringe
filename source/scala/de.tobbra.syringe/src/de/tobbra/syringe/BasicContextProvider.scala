package de.tobbra.syringe

trait BasicContextProvider { self: Factory =>
    val context = new BasicContext {
        val factory: Factory = BasicContextProvider.this
        val singletonScope = new CachingScope(true)
        val threadLocalScope = new ThreadLocalScope
        val transientScope = TransientScope
    }
}