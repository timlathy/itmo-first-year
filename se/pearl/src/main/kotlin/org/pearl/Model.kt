package org.pearl

import org.pearl.query.WherePredicate

@Target(AnnotationTarget.PROPERTY)
annotation class Id

abstract class Model {
  open val tableName: String = javaClass.simpleName

  val schema: Schema = Schemas.ofModel(javaClass)

  operator fun get(column: String) = WherePredicate.Column(tableName, column)
}
