package beethoven

case class AcceleratorSystemConfig(nCores: Int,
                                   name: String,

                                   /** In elements, per write channel, scaled by the number of bytes
                         */
                                   moduleConstructor: ModuleConstructor,
                                   memoryChannelConfig: List[MemChannelConfig] = List(),
                                   canReceiveSoftwareCommands: Boolean = true,
                                   canIssueCoreCommandsTo: Seq[String] = Seq.empty,
                                   canSendDataTo: Seq[String] = Seq.empty
                       )