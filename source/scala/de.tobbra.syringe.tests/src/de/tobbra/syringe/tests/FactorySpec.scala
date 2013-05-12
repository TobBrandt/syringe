package de.tobbra.syringe.tests

import org.scalatest._
import scala.concurrent._
import scala.concurrent.duration._
import de.tobbra.syringe._
import de.tobbra.syringe.bindings._
import de.tobbra.syringe.modules._
import java.util.concurrent.Executors
import scala.reflect.runtime.universe._

class FactorySpec extends FlatSpec with ShouldMatchers {

    implicit val exeCtx = ExecutionContext.fromExecutor(Executors.newCachedThreadPool)

    "factory" should "return same instance in singleton scope" in {
        val factory = Factory(Module1)
        val service1 = factory.get[ServiceA]
        val service2 = factory.get[ServiceA]
        service1 should be theSameInstanceAs (service2)
    }

    it should "return different instances in transient scope" in {
        val factory = Factory(Module1Trans)
        val service1 = factory.get[ServiceA]
        val service2 = factory.get[ServiceA]
        service1 should not be theSameInstanceAs(service2)
    }

    it should "return same instance in thread scope" in {
        val factory = Factory(Module1Thread)
        val service1 = factory.get[ServiceA]
        val service2 = factory.get[ServiceA]
        service1 should be theSameInstanceAs (service2)
    }

    it should "be able to resolve nested bindings" in {
        val factory = Factory(Module1)
        val serviceA = factory.get[ServiceA]
        val serviceB = factory.get[ServiceB]
        serviceA should be theSameInstanceAs (serviceB.sa)
    }

    it should "be able to bind types at compile time" in {
        val factory = Factory(Module2)
        val service1 = factory.get[ServiceA]
        val service2 = factory.get[ServiceA]
        service1 should be theSameInstanceAs (service2)
    }

    "factory accessed by different threads" should "return different instances in thread scope" in {
        val factory = Factory(Module1Thread)
        val service1 = factory.get[ServiceA]
        val service2 = spawnBlocking(factory.get[ServiceA])
        service1 should not be theSameInstanceAs(service2)
    }

    it should "return the same instance in singleton scope" in {
        val factory = Factory(Module1)
        val service1 = factory.get[ServiceA]
        val service2 = spawnBlocking(factory.get[ServiceA])
        service1 should be theSameInstanceAs (service2)
    }

    private def spawnBlocking[T](x: => T): T = {
        val fx = future(x)
        Await.result(fx, Duration.Inf)
        fx.value.get.get
    }
}

trait ServiceA {
    def a: String
}

trait ServiceB {
    def b: String
    def sa: ServiceA
}

class ServiceAImpl1 extends ServiceA {
    def a = "ServiceAImpl1"
}

case class ServiceBImpl1(val sa: ServiceA) extends ServiceB {
    def b = sa.a
}

object Module1 extends Module.Basic with ReflectiveBindings {
    def name = "Module1"
    bindProvider[ServiceA]((c: Ctx) => new ServiceAImpl1, (c: Ctx) => c.singletonScope)
    bindSingleton[ServiceB, ServiceBImpl1]
}

object Module1Trans extends Module.Basic with ReflectiveBindings {
    def name = "Module1Trans"
    bindTransient[ServiceA, ServiceAImpl1]
}

object Module1Thread extends Module.Basic with ReflectiveBindings {
    def name = "Module1Thread"
    bindThreadLocal[ServiceA, ServiceAImpl1]
}

object Module2 extends Module.Basic with MacroBindings {
    def name = "Module2"
    bindSingleton[ServiceA, ServiceAImpl1]
}