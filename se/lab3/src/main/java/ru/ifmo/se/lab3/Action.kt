package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
@Inheritance(strategy = InheritanceType.JOINED)
abstract class Action(
    @ManyToOne
    @JoinColumn(name="actor_id")
    val actor: Person,

    @Id @GeneratedValue
    val id: Long = -1)
