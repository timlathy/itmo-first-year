package ru.ifmo.se.lab3

import javax.persistence.*

@Entity
class ShoutedRemark(text: String, speaker: Character) : Remark(text, speaker)
