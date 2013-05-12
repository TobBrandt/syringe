package de.tobbra.syringe.macros

object MacroBindingsHelper {

    import scala.reflect.macros

    def makeProvider[P: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag](c: macros.Context): c.Expr[C => P] = {
        import c.universe._
        val p = implicitly[WeakTypeTag[P]].tpe
        val b = implicitly[WeakTypeTag[B]].tpe
        require(b <:< p, s"Bound type $b must be a subtype of provided type $p")

        val tsym = b.typeSymbol
        require(tsym.isClass, s"Bound type $b (providing $p) must be a class.")
        val ctorSym = b.declaration(nme.CONSTRUCTOR).asTerm.alternatives.collectFirst {
            case s: MethodSymbol if s.isPrimaryConstructor => s
        }
        require(!ctorSym.isEmpty, s"Bound type $b must have a primary constructor.")
        val paramTypes = ctorSym.get.paramss(0).map(_.typeSignature.typeSymbol)

        var ctxName = newTermName(c.fresh("ctx"))
        def useFactory(s: Symbol): Tree =
            Apply(
                Select(
                    Select(Ident(ctxName), newTermName("factory")),
                    newTermName("get")),
                List(Ident(s)))

        val args = paramTypes.map(useFactory(_))
        val ccall = New(tsym, args: _*)
        var result = Function(
            List(ValDef(Modifiers(), ctxName, Ident(implicitly[WeakTypeTag[C]].tpe.typeSymbol), EmptyTree)),
            ccall)
        c.Expr[C => P](result)
    }

    def bindInScope[P: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, S: c.WeakTypeTag](c: macros.Context)(scope: c.Expr[C => S]): c.Expr[Unit] = {
        import c.universe._
        val provider = makeProvider[P, B, C](c).tree
        c.Expr[Unit](Apply(Ident(newTermName("bindProvider")), List(provider, scope.tree)))
    }

    def bindSingleton[P: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, S: c.WeakTypeTag](c: macros.Context): c.Expr[Unit] = {
        bindInScope[P, B, C, S](c)(selectScope[C, S](c, "singletonScope"))
    }

    def bindTransient[P: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, S: c.WeakTypeTag](c: macros.Context): c.Expr[Unit] = {
        bindInScope[P, B, C, S](c)(selectScope[C, S](c, "transientScope"))
    }

    def bindThreadLocal[P: c.WeakTypeTag, B: c.WeakTypeTag, C: c.WeakTypeTag, S: c.WeakTypeTag](c: macros.Context): c.Expr[Unit] = {
        bindInScope[P, B, C, S](c)(selectScope[C, S](c, "threadLocalScope"))
    }

    private def selectScope[C: c.WeakTypeTag, S: c.WeakTypeTag](c: macros.Context, scope: String) = {
        import c.universe._
        var ctx = newTermName(c.fresh("ctx"))
        c.Expr[C => S](
            Function(
                List(ValDef(Modifiers(), ctx, Ident(implicitly[WeakTypeTag[C]].tpe.typeSymbol), EmptyTree)),
                Select(Ident(ctx), newTermName(scope))))
    }
}