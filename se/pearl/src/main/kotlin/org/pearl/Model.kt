package org.pearl

import org.pearl.query.WherePredicate

@Target(AnnotationTarget.PROPERTY)
annotation class IdColumn

abstract class Model {
  operator fun get(column: String) = WherePredicate.Column(tableName, column)

  open val tableName: String = javaClass.simpleName
}
