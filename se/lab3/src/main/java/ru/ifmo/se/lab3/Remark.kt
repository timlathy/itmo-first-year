package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
@Inheritance(strategy = InheritanceType.JOINED)
abstract class Remark(
    val text: String,

    @ManyToOne
    @JoinColumn(name="speaker_id")
    val speaker: Character,

    @Id @GeneratedValue
    val id: Long = -1)
