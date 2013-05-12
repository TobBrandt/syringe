package de.tobbra.syringe

import scala.collection.mutable
import scala.reflect.runtime.universe._
import de.tobbra.syringe.bindings._
import de.tobbra.syringe.modules._
import de.tobbra.syringe._

abstract class DefaultFactory[C <: Context](val modules: Module[C]*) extends Factory {
    val context: C

    private val bindings = mutable.Map[Type, (Binding[C], String)]()

    for {
        m <- modules
        b <- m.bindings
        t = b.providedType.map(_.normalize)
    } yield if (bindings.contains(t)) {
        val other = bindings.get(t).get
        val otherName = other._2
        if (otherName == m.name)
            throw new Error(s"Duplicate binding for type $t in module ${m.name}")
        else
            throw new Error(s"Conflicting binding for type $t in modules $otherName and ${m.name}")
    } else {
        bindings.put(t, (b, m.name))
    }

    def provide(t: Type): () => Any = {
        val candidates = bindings.keys.filter(_ <:< t).toIndexedSeq
        if (candidates.isEmpty) throw new Error(s"Found no binding for type $t.")
        val exactMatches = candidates.filter(_ =:= t).toIndexedSeq
        if (exactMatches.size == 1)
            return makeProvider(t, bindings.get(exactMatches(0)).get._1)
        if (candidates.size > 1) {
            def fmtBinding(t: Type, v: (Binding[C], String)) =
                s"$t -> ${v._1.providedType.map(_.normalize)} in '${v._2}'"
            val candidateList = candidates.map(c => fmtBinding(c, bindings.get(c).get))
            throw new Error(s"Ambigous binding for $t, candidates are: $candidateList.")
        }
        makeProvider(t, bindings.get(candidates(0)).get._1)
    }

    private def makeProvider(t: Type, b: Binding[C]): () => Any = {
        val scope = b.scope(context)
        () => scope.get(t, b.provider(context))
    }
}