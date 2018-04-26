package com.gildedrose

import org.scalatest._

class GildedRoseTest extends WordSpec with Matchers {

  val BASE_QUALITY = 25
  val BASE_SELL_IN = 20
  val MAX_QUALITY = 50
  val SULFURAS_QUALITY = 80

  trait StandardItem {
    val items = Array(new Item(name = "Standrad Item", quality = BASE_QUALITY, sellIn = BASE_SELL_IN))
    val app = new GildedRose(items)
  }

  trait AgedBrie {
    val items = Array(new Item(name = "Aged Brie", quality = BASE_QUALITY, sellIn = BASE_SELL_IN))
    val app = new GildedRose(items)
  }

  trait Sulfuras {
    val items = Array(new Item(name = "Sulfuras, Hand of Ragnaros", quality = SULFURAS_QUALITY, sellIn = BASE_SELL_IN))
    val app = new GildedRose(items)
  }

  trait BackstagePass {
    val items = Array(new Item(name = "Backstage passes to a TAFKAL80ETC concert", quality = BASE_QUALITY, sellIn = BASE_SELL_IN))
    val app = new GildedRose(items)
  }

  "A single item" when {
    "the day ends" should {
      "decrease its quality" in new StandardItem {
        app.updateQuality()

        items.head.quality should be(BASE_QUALITY - 1)
      }
      "decrease its sellIn value" in new StandardItem {
        app.updateQuality()

        items.head.sellIn should be(BASE_SELL_IN - 1)
      }
      "decrease its quality twice as fast once the sell by date has passed" in new StandardItem {
        items.head.sellIn = 0

        app.updateQuality()

        items.head.quality should be(BASE_QUALITY - 2)
      }
      "the quality of an item is never negative" in new StandardItem {
        items.head.quality = 0

        app.updateQuality()

        items.head.quality should be(0)
      }
      "The quality of an item is never more than 50" in new AgedBrie {
        items.head.quality = MAX_QUALITY

        app.updateQuality()

        items.head.quality should be(MAX_QUALITY)
      }
    }
  }

  "Aged Brie" when {
    "the day ends" should {
      "increases its quality the older it gets" in new AgedBrie {
        app.updateQuality()

        items.head.quality should be(BASE_QUALITY + 1)
      }
      "increase its quality after the sell by date" in new AgedBrie {
        items.head.sellIn = 0

        app.updateQuality()

        items.head.quality should be(BASE_QUALITY + 2)
      }
    }
  }

  "Sulfuras" when {
    "the day ends" should {
      "never be sold" in new Sulfuras {
        app.updateQuality()

        items.head.sellIn should be(BASE_SELL_IN)
      }
      "never decrease its quality" in new Sulfuras {
        app.updateQuality()

        items.head.quality should be(SULFURAS_QUALITY)
      }
    }
  }

  "A backstage pass" when {
    "the day ends" should {
      "increase its quality the older it gets" in new BackstagePass {
        app.updateQuality()

        items.head.quality should be(BASE_QUALITY + 1)
      }
      "increase its quality twice as fast 10 days before the concert" in new BackstagePass {
        items.head.sellIn = 10

        app.updateQuality()

        items.head.quality should be(BASE_QUALITY + 2)
      }
      "increase its quality by 3 @ 5 days before the concert" in new BackstagePass {
        items.head.sellIn = 5

        app.updateQuality()

        items.head.quality should be(BASE_QUALITY + 3)
      }
      "drop its quality to 0 after the concert" in new BackstagePass {
        items.head.sellIn = 0

        app.updateQuality()

        items.head.quality should be(0)
      }
    }
  }
}