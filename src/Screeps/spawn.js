exports.spawnCreep_ = (spawnId, bodyTypes, creepName, opts) => () => Game.spawns[spawnId].spawnCreep(bodyTypes, creepName, {})

exports.spawns_ = () => Object.keys(Game.spawns)

exports.gameNotify_ = (message, groupInterval) => () => Game.notify(message, groupInterval)

exports.getRawMemory_ = () => RawMemory.get()
exports.setRawMemory_ = (memory) => () => RawMemory.set(memory)