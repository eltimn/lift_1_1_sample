package com.liftcode.model

import net.liftweb._
import mapper._

class Dog extends LongKeyedMapper[Dog] with IdPK {
  def getSingleton = Dog

  object name extends MappedPoliteString(this, 128)
  object weight extends MappedInt(this)
}

object Dog extends Dog with LongKeyedMetaMapper[Dog] {
	override def dbDefaultConnectionIdentifier = bootstrap.liftweb.TwoDB
}