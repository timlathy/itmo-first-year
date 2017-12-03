package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
data class BankAccount(
    val balance: Int,

    @Id @GeneratedValue
    val id: Long = -1)
