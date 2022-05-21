package composer

import freechips.rocketchip.config.Field

// Number of external DDR channels
case object NMemChan extends Field[Int] // ddr channels 

//noinspection DuplicatedCode
case object ProducerBuffers extends Field[Map[Seq[(Int, Int)], Seq[(Int, Int)]]]

//noinspection DuplicatedCode
case object ConsumerBuffers extends Field[Map[Seq[(Int, Int)], Seq[(Int, Int)]]]

case object ComposerSystemsKey extends Field[Seq[ComposerSystemParams]]


/* TODO UG: How many bits we are using to identify a system. Ensure that no system has the same ID and the largest ID
 *           can be represented with this many bits
 */
case object SystemIDLengthKey extends Field[Int]

/* TODO UG: How many bits are we using to identify a core. Ensure that no system has more cores than can be uniquely
 *           identified by this many bits.
 */
case object CoreIDLengthKey extends Field[Int]
