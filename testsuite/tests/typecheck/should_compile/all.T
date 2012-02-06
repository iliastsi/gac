
test('tc001', normal, compile, ['-fwarn-type-overflows'])
test('tc002', normal, compile, [''])
test('tc003', normal, compile, ['-fwarn-unreachable-code -ferror-spans'])
