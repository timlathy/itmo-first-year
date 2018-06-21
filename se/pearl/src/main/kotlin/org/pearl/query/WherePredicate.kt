package org.pearl.query

import org.pearl.query.WherePredicate.Binary.BinaryOp.*
import org.pearl.query.WherePredicate.BinaryMatch.MatchOp.*
import org.pearl.query.WherePredicate.UnaryMatch.UnaryMatchOp.*

fun not(operand: WherePredicate) = WherePredicate.Unary(WherePredicate.Unary.UnaryOp.NOT, operand)

sealed class WherePredicate {
  class Column(val table: String, val column: String) {
    infix fun eq(value: Any) = WherePredicate.BinaryMatch(EQ, table, column, value)
    infix fun notEq(value: Any) = WherePredicate.BinaryMatch(NOT_EQ, table, column, value)
    infix fun lt(value: Any) = WherePredicate.BinaryMatch(LT, table, column, value)
    infix fun gt(value: Any) = WherePredicate.BinaryMatch(GT, table, column, value)
    infix fun `in`(value: Any) = WherePredicate.BinaryMatch(IN, table, column, value)

    fun isNull() = WherePredicate.UnaryMatch(IS_NULL, table, column)
    fun isNotNull() = WherePredicate.UnaryMatch(IS_NOT_NULL, table, column)
  }

  abstract val bindings: List<Any>

  infix fun or(right: WherePredicate) = Binary(OR, this, right)
  infix fun and(right: WherePredicate) = Binary(AND, this, right)

  data class Unary(val op: UnaryOp, val operand: WherePredicate): WherePredicate() {
    enum class UnaryOp(val sql: String) {
      NOT("NOT");

      override fun toString() = sql
    }

    override val bindings = operand.bindings

    override fun toString() = "$op $operand"
  }

  data class Binary(val op: BinaryOp, val left: WherePredicate, val right: WherePredicate): WherePredicate() {
    enum class BinaryOp(val sql: String) {
      AND("AND"), OR("OR");

      override fun toString() = sql
    }

    override val bindings = left.bindings + right.bindings

    override fun toString() = "($left $op $right)"
  }

  data class UnaryMatch(val op: UnaryMatchOp, val table: String, val column: String): WherePredicate() {
    enum class UnaryMatchOp(val sql: String) {
      IS_NULL("IS NULL"), IS_NOT_NULL("IS NOT NULL");

      override fun toString() = sql
    }

    override val bindings = emptyList<Any>()

    override fun toString() = "\"$table\".\"$column\" $op"
  }

  data class BinaryMatch(val op: MatchOp, val table: String, val column: String, val value: Any): WherePredicate() {
    enum class MatchOp(val sql: String) {
      EQ("="), NOT_EQ("!="), LT("<"), GT(">"), LTE("<="), GTE(">="), IN("IN");

      override fun toString() = sql
    }

    override val bindings = when (value) {
      is SelectQuery<*> -> value.toParameterizedSql().second
      else -> listOf(value)
    }

    override fun toString() = "\"$table\".\"$column\" $op " +
      (if (value is SelectQuery<*>) "(" + value.toParameterizedSql().first + ")" else "?")
  }
}
