package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
data class Person(
    val name: String,
    @Id @GeneratedValue
    val id: Long = -1)
