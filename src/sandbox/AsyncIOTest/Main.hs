
import Control.Concurrent.Async (Async, async, wait)
import System.IO (withFile, IOMode(..), hPutStrLn, Handle)

makeAsync :: Int -> IO (Async ())
makeAsync idx = async $ do
    let p = product [1..10000] :: Integer
    withFile ("./tmp_" ++ show idx ++ ".log") WriteMode $ \h -> do
        hPutStrLn h $ show p

main :: IO ()
main = do
    tests <- mapM makeAsync [1..50]
    mapM_ wait tests

{- time ./out +RTS -N2 -s
      75,022,900 bytes allocated in the heap
         886,268 bytes copied during GC
         955,444 bytes maximum residency (3 sample(s))
         449,188 bytes maximum slop
               4 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0       137 colls,   137 par    0.09s    0.13s     0.0010s    0.0248s
  Gen  1         3 colls,     2 par    0.00s    0.00s     0.0004s    0.0005s

  Parallel GC work balance: 7.95% (serial 0%, perfect 100%)

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.17s  (  0.19s elapsed)
  GC      time    0.09s  (  0.13s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.26s  (  0.32s elapsed)

  Alloc rate    439,136,160 bytes per MUT second

  Productivity  65.4% of total user, 53.6% of total elapsed

gc_alloc_block_sync: 6
whitehole_spin: 0
gen[0].sync: 5
gen[1].sync: 0

real    0m0.329s
user    0m0.263s
sys     0m0.055s

----

            75,386,240 bytes allocated in the heap
       1,021,372 bytes copied during GC
       1,180,120 bytes maximum residency (3 sample(s))
         438,456 bytes maximum slop
               4 MB total memory in use (0 MB lost due to fragmentation)

                                    Tot time (elapsed)  Avg pause  Max pause
  Gen  0       137 colls,   137 par    0.01s    0.01s     0.0000s    0.0009s
  Gen  1         3 colls,     2 par    0.00s    0.00s     0.0005s    0.0010s

  Parallel GC work balance: 11.98% (serial 0%, perfect 100%)

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N2)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.00s  (  0.00s elapsed)
  MUT     time    0.09s  (  0.09s elapsed)
  GC      time    0.01s  (  0.01s elapsed)
  EXIT    time    0.00s  (  0.00s elapsed)
  Total   time    0.10s  (  0.09s elapsed)

  Alloc rate    822,141,229 bytes per MUT second

  Productivity  91.7% of total user, 98.6% of total elapsed

gc_alloc_block_sync: 255
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 1

real    0m0.101s
user    0m0.101s
sys     0m0.024s
-}

