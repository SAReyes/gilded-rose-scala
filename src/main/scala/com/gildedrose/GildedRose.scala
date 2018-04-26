package com.gildedrose

class GildedRose(val items: Array[Item]) {

  import GildedRose._

  def updateQuality() {
    for (i <- items.indices) {
      if (items(i).deterioratesOverTime) {
        items(i).decreaseQuality()
      } else if (items(i).improvesOverTime) {
        items(i).increaseQuality()
      }

      if (!items(i).name.equals(SULFURAS)) {
        items(i).sellIn = items(i).sellIn - 1
      }

      if (items(i).sellIn < 0) {
        if (!items(i).name.equals(AGED_BRIE)) {
          if (!items(i).name.equals(BACKSTAGE_TICKET)) {
            if (items(i).quality > 0) {
              if (!items(i).name.equals(SULFURAS)) {
                items(i).decreaseQuality()
              }
            }
          } else {
            items(i).quality = items(i).quality - items(i).quality
          }
        } else {
          items(i).increaseQuality()
        }
      }
    }
  }

  implicit def extendItem(item: Item): ExtendedItem = new ExtendedItem(item)
}

object GildedRose {
  val AGED_BRIE = "Aged Brie"
  val SULFURAS = "Sulfuras, Hand of Ragnaros"
  val BACKSTAGE_TICKET = "Backstage passes to a TAFKAL80ETC concert"
}

class ExtendedItem(value: Item) {

  import GildedRose._

  def increaseQuality(): Unit = {
    def innerIncrease(): Unit = {
      value.quality = if (value.quality == 50) 50 else value.quality + 1
    }

    innerIncrease()

    if (value.name == BACKSTAGE_TICKET) {
      if (value.sellIn < 11) {
        innerIncrease()
      }

      if (value.sellIn < 6) {
        innerIncrease()
      }
    }
  }

  def decreaseQuality(): Unit = {
    value.quality = if (value.quality == 0) 0 else value.quality - 1
  }

  def isLegendary: Boolean = value.name == SULFURAS
  def improvesOverTime: Boolean = value.name == AGED_BRIE || value.name == BACKSTAGE_TICKET
  def deterioratesOverTime: Boolean = isNotLegendary && !improvesOverTime
  def isNotLegendary: Boolean = !isLegendary
}