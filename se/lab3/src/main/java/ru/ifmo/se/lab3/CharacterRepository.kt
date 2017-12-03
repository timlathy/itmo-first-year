package ru.ifmo.se.lab3

import org.springframework.data.repository.CrudRepository

interface CharacterRepository : CrudRepository<Character, Long> {
  fun findByName(name: String): Character
}
