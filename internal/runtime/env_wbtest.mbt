///|
test "panic env define empty" {
  let empty : @core.Env = []
  env_define(empty, "x", Void)
}
