module Main

import Hardware.MOS6502.Emu
import System.File.Buffer
import Data.Buffer

bufMachine : Buffer -> Machine
bufMachine buf = MkMachine
  { readMem_ = \addr => getBits8 buf (cast addr)
  , writeMem_ = \addr => setBits8 buf (cast addr)
  }

untilIO : acc -> (acc -> IO (Either acc a)) -> IO a
untilIO acc0 step = fromPrim $ go acc0
  where
    go : acc -> PrimIO a
    go acc w =
      let MkIORes (Left acc') w' = toPrim (step acc) w
            | MkIORes (Right res) w' => MkIORes res w'
      in go acc' w'

single : Machine => CPU => IO (Maybe Addr)
single = do
  before <- getReg pc
  step
  after <- getReg pc
  -- PC hasn't changed (`jmp *`) ==> we're at an error trap
  pure $ if before == after then Just after else Nothing

run : Buffer -> IO (Nat, Addr)
run buf = do
  cpu <- new 0x0400
  let m = bufMachine buf

  untilIO 0 $ \cnt => do
    Nothing <- single
      | Just end => pure $ Right (cnt, end)
    pure $ Left $ cnt + 1

main : IO ()
main = do
  Right buf <- createBufferFromFile "6502_functional_test.bin"
    | Left err => printLn err
  (cnt, pc) <- run buf
  printLn (cnt, pc)
