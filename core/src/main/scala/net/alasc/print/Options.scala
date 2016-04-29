package net.alasc.print

class Options(val map: Map[Options.Key, Any]) extends AnyVal {

  def apply(k: Options.Key): k.Value = map.getOrElse(k, k.defaultValue).asInstanceOf[k.Value]

  def +(option: Options.KeyValue[_ <: Options.Key]): Options = new Options(map + ((option.key, option.value)))

  def ++(options: Options): Options = new Options(map ++ options.map)
}

object Options {

  def apply(options: Options.KeyValue[_ <: Options.Key]*): Options = new Options(options.map(kv => ((kv.key, kv.value))).toMap)

  def empty: Options = new Options(Map.empty[Options.Key, Any])

  abstract class Key(val name: String) extends AnyRef {

    type Value

    override def hashCode = name.hashCode

    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

    def defaultValue: Value

    def := (value: Value): KeyValue[this.type] = KeyValue[this.type](this, value)

  }

  case class KeyValue[K <: Key with Singleton](key: K, value: K#Value)

  def Key[Value0](name: String, defaultValue0: Value0): Key { type Value = Value0 } = new Key(name) {

    type Value = Value0

    def defaultValue = defaultValue0

  }

}
