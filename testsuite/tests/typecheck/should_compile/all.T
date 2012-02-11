
test('tc001', normal, compile, ['-fwarn-type-overflows'])
test('tc002', normal, compile, [''])
test('tc003', normal, compile, ['-fwarn-unreachable-code -ferror-spans'])
test('tc004', normal, compile, ['-fwarn-unused-function -ferror-spans'])
test('tc005', normal, compile, ['-fwarn-unused-variable -ferror-spans'])
test('tc006', normal, compile, ['-fwarn-unused-result -ferror-spans'])
