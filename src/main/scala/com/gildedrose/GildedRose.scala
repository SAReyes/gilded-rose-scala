package com.gildedrose

class GildedRose(val items: Array[Item]) {
  import GildedRose._

  def updateQuality() {
    for (i <- items.indices) {
      if (!items(i).name.equals(AGED_BRIE)
        && !items(i).name.equals(BACKSTAGE_TICKET)) {
        if (items(i).quality > 0) {
          if (!items(i).name.equals(SULFURAS)) {
            items(i).quality = items(i).quality - 1
          }
        }
      } else {
        if (items(i).quality < 50) {
          items(i).quality = items(i).quality + 1

          if (items(i).name == BACKSTAGE_TICKET) {
            if (items(i).sellIn < 11) {
              if (items(i).quality < 50) {
                items(i).quality = items(i).quality + 1
              }
            }

            if (items(i).sellIn < 6) {
              if (items(i).quality < 50) {
                items(i).quality = items(i).quality + 1
              }
            }
          }
        }
      }

      if (!items(i).name.equals(SULFURAS)) {
        items(i).sellIn = items(i).sellIn - 1
      }

      if (items(i).sellIn < 0) {
        if (!items(i).name.equals(AGED_BRIE)) {
          if (!items(i).name.equals(BACKSTAGE_TICKET)) {
            if (items(i).quality > 0) {
              if (!items(i).name.equals(SULFURAS)) {
                items(i).quality = items(i).quality - 1
              }
            }
          } else {
            items(i).quality = items(i).quality - items(i).quality
          }
        } else {
          if (items(i).quality < 50) {
            items(i).quality = items(i).quality + 1
          }
        }
      }
    }
  }
}

object GildedRose {
  val AGED_BRIE = "Aged Brie"
  val SULFURAS = "Sulfuras, Hand of Ragnaros"
  val BACKSTAGE_TICKET = "Backstage passes to a TAFKAL80ETC concert"
}