package net.alasc.print

/** Type-safe map of pretty-printing options, inspired by the SBT key-value map. */
class Options(val map: Map[Options.Key, Any]) extends AnyVal {

  /** Returns the value of the provided key if the map contains it, otherwise its default value. */
  def apply(k: Options.Key): k.Value = map.getOrElse(k, k.defaultValue).asInstanceOf[k.Value]

  /** Returns the updated [[Options]] map with the given key-value pair. */
  def +(option: Options.KeyValue[_ <: Options.Key]): Options = new Options(map + ((option.key, option.value)))

  /** Returns a new [[Options]] map with the elements of the given [[Options]] map added/updated to this map. */
  def ++(options: Options): Options = new Options(map ++ options.map)

}

object Options {

  /** Creates an options map with the given key-value pairs. */
  def apply(options: Options.KeyValue[_ <: Options.Key]*): Options = new Options(options.map(kv => ((kv.key, kv.value))).toMap)

  /** Returns an empty options map. */
  def empty: Options = new Options(Map.empty[Options.Key, Any])

  /** Base class for the options keys, that should be singleton objects extending this. Instances of keys are compared
    * by their reference using `eq`.
    *
    * @param name Key name, from the key `hashCode` is derived.
    */
  abstract class Key(val name: String) extends AnyRef { // TODO: derive the name from a macro, "enclosingObject.instanceName"

    /** Type for the value corresponding to this key. */
    type Value

    /** Key hashCode, derived from its name. */
    override val hashCode = name.hashCode

    /** Keys are compared by reference (see [[eq]]). */
    override def equals(that: Any): Boolean = this eq that.asInstanceOf[AnyRef]

    /** Default value for that key, in case the key is not defined in the [[Options]] map. */
    def defaultValue: Value

    /** Assigns a given value to this key and returns a [[KeyValue]] pair object maintaining the type safety
      * that the type of the value corresponds to [[Value]]. */
    def := (value: Value): KeyValue[this.type] = KeyValue[this.type](this, value)

  }

  /** Type-safe key-value object.
    *
    * @tparam K     Singleton type of the key.
    *
    * @param  key   Key instance corresponding to `K`.
    * @param  value Value of type corresponding to the key.
    */
  case class KeyValue[K <: Key with Singleton](key: K, value: K#Value)

}
