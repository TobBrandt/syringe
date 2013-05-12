package de.tobbra.syringe

import scala.reflect.runtime.universe.{ Type, TypeTag, typeOf }
import scala.collection.mutable

trait Scope {

    /**
     * Get a remembered instance or construct and remember a new one.
     */
    final def get[T: TypeTag](construct: => T): T = get(typeOf[T], construct).asInstanceOf[T]
    def get(t: Type, construct: => Any): Any
}

object TransientScope extends Scope {
    def get(t: Type, construct: => Any): Any = construct
}

class CachingScope(val threadSafe: Boolean) extends Scope {
    private val cache = mutable.Map[Type, Any]()

    def get(t: Type, construct: => Any): Any = {
        if (threadSafe) cache.synchronized(getInternal(t, construct))
        else getInternal(t, construct)
    }

    private def getInternal(t: Type, construct: => Any): Any = {
        cache.getOrElseUpdate(t.map(_.normalize), construct)
    }
}

class ThreadLocalScope extends Scope {
    private val scopes = new java.lang.ThreadLocal[Scope] {
        override def initialValue = new CachingScope(true)
    }

    def get(t: Type, construct: => Any): Any = scopes.get.get(construct)
}