namespace GeneticLbp

open Biometrics
open System
open System.Threading

module PArray = Array.Parallel

type AppSettings = FSharp.Configuration.AppSettings<"App.config">
type Ratio = Over of int * int

type Laws =
    {
        CountLimit:int
        MutationProbability:Ratio
        PopulationSize:int
        WiseElders:int
        ElitePercentage:float32<percent>
        EliteCount:int
    }

    /// Return a shrinkage factor penalizing complexity (length) of a genome.
    member x.Punish f =
        let f = float32 f
        pown (f - 1.f / sqrt (float32 x.CountLimit * 0.618f)) 2 + 1.f

type Universe(train:obj[], test:obj[], ?laws) =
    static let splitArray = LbpArea.Splitter.ToCharArray()
    static let digits = [| '0'..'9' |]
    static let flags = LbpArea.FLAG_CHARS.ToCharArray()
    static let mutatableChars = Array.append digits flags

    let trace = System.Diagnostics.Trace.WriteLine

    let laws =
        let (-/) n d = Over(n, d)
        let laws = defaultArg laws {
            CountLimit = AppSettings.MaxAreaCountPerIndividual
            MutationProbability = AppSettings.MutationProbabilityNumerator -/ AppSettings.MutationProbabilityDenominator
            PopulationSize = AppSettings.PopulationSize
            WiseElders = AppSettings.WiseElders
            ElitePercentage = float32 AppSettings.ElitesToKeep * 1.0f<percent>
            EliteCount = 0
        }
        { laws with EliteCount = float32 laws.PopulationSize * laws.ElitePercentage |> int }

    let width = 360
    let height = 720

    /// Thread-local random generator.
    let random =
        let now = DateTime.Now
        new ThreadLocal<_>(fun _ -> KissRandom(uint32 now.Year, uint32 now.Month, uint32 now.Day, uint32 now.Ticks))

    /// Thread-local Gaussian normal distribution (0 mean and standard deviation 1).
    let normal =
        new ThreadLocal<_>(fun _ ->
            let kr = KissRandom(random.Value.Next())
            kr.Normal().GetEnumerator()
        )

    /// A collection of valid LBP parameter areas.
    let jumpStart =
        let rects = [
            yield 1, 1, width - 2, height - 2
            for d = 2 to 6 do
                let w = width / d
                let h = height / d
                for i = 0 to d - 1 do
                    yield i * w, i * h, w, h
        ]
        [|
            for r in rects |> List.map Rectangle do
                yield LbpArea(area = r, radius = 1uy)
                yield LbpArea(area = r, radius = 1uy, rotation = true)
                yield LbpArea(area = r, radius = 1uy, symmetric = true)
                yield LbpArea(area = r, radius = 1uy, uniform = true)
                yield LbpArea(area = r, radius = 2uy, uniform = true)
        |]

    /// Assess the fitness of a genome with a float (higher is better).
    let fitness =
        let damp = 1.f<fit/percent> / laws.Punish test.Length
        let fitness (parameters:LbpArea[]) =
            //trace "fitness()"
            let uniforms = (parameters |> Array.sumBy (fun a -> if a.uniform then 1 else 0) |> float32)
            let percentage = 1.f<percent> * uniforms / float32 parameters.Length
            percentage, percentage * damp
        fitness

    /// Possibly mutate a genome.
    let mutate =
        let (Over(nom, denum)) = laws.MutationProbability
        /// Mutate a flag, e.g. "0123u56789" to "0123s56789".
        let mutateFlag pos (gen:string) =
            assert(flags |> Array.contains gen.[pos])
            gen.Substring(0, pos) + string flags.[random.Value.Next flags.Length] + gen.Substring(pos + 1)
            |> Genome
        let mutate genome =
            //trace "mutate()"
            match random.Value.Next denum, genome with
            | n, Genome gen when n < nom ->
                let startLooking = random.Value.Next gen.Length
                let startMutating = gen.IndexOfAny(mutatableChars, startLooking)
                if startMutating < 0 then genome else
                if flags |> Array.contains gen.[startMutating] then
                    mutateFlag startMutating gen
                else
                    genome
            | _ -> genome
        mutate

    /// Given two parents breed two children by splitting and exchange the second half of DNA and mutating.
    let breed =
        let split (Genome genome) =
            let len = genome.Length |> float32
            let split =
                let valid = normal.Value.MoveNext()
                assert(valid)
                normal.Value.Current * 0.25f * len + 0.5f * len + 0.5f |> int
            let split = max 0 (min split genome.Length - 1)
            let semicolon = genome.IndexOf(LbpArea.Splitter.[0], split)
            if semicolon > 0 then
                genome.Substring(0, semicolon), genome.Substring(semicolon + 1)
            else
                genome.Substring(0, split), genome.Substring(split + 1)
        fun father mother ->
            //trace "breed()"
            let fa, fb = split father
            let ma, mb = split mother
            let child1 = String.Concat(fa, mb)
            let child2 = String.Concat(fb, ma)
            mutate (Genome child1), mutate (Genome child2)

    /// De-serialize a genome string to an array of parameter areas.
    let genomeToArray = LbpArea.GenomeToArray splitArray >> Array.filter (fun area ->
            let c = area.covers
            c.x + c.width <= width && c.y + c.height <= height
        )

    /// Serialize an array of parameter areas to a genome string.
    let arrayToGenome = LbpArea.SerializeGenome >> Genome

    /// Generate an array (of given length) of life forms with probabilistically increasing DNA complexity.
    let lengthRandomlyIncreasing size =
        trace "lengthRandomlyIncreasing()"
        PArray.init size (fun s ->
            Array.init (random.Value.Next s) (fun _ -> jumpStart.[random.Value.Next jumpStart.Length]))
        |> PArray.map arrayToGenome

    let accessFitness = fst >> snd

    let breedPopulation (rated:((_ * float32<fit>) * LbpArea []) []) =
        trace "breedPopulation()"
        if rated.Length > 0 then
            let wmax:float32</fit> =
                let theMax = Array.maxBy accessFitness rated |> accessFitness
                1.f / theMax
            /// Randomly chooses one of the fit individuals with probability proportional to fitness.
            let rec chooser () =
                let selected = random.Value.Next rated.Length
                if random.Value.NextSingle() > accessFitness rated.[selected] * wmax then chooser() else
                rated.[selected] |> snd |> arrayToGenome

            let parents =
                let parentsToBreed = (laws.PopulationSize - laws.WiseElders - laws.EliteCount) / 2 + random.Value.Next 2
                PArray.init parentsToBreed (fun _ -> chooser(), chooser())
            let children =
                parents
                |> PArray.collect (fun (father, mother) ->
                    let c, d = breed father mother
                    [| c; d |]
                )

            let leaders =  rated |> Array.take (min rated.Length laws.WiseElders) |> Array.map (snd >> arrayToGenome)

            PArray.init laws.EliteCount (fun _ -> chooser())
            |> Array.append leaders
            |> Array.append children
        else
            lengthRandomlyIncreasing laws.PopulationSize

    /// Start life simulation.
    member x.Simulate (pleaseStop:ManualResetEvent) =
        let simulate () =
            let rec simulate generation (lastTime:DateTime) ewma (population:Genome[]) =
                generation |> sprintf "simulate() gen %d" |> trace
                /// These can be bread, i.e. the genome is interpretable.
                let breedable =
                    population
                    |> PArray.map genomeToArray
                /// These can be born, i.e. fitness > 0.
                let alive =
                    breedable
                    |> PArray.map (fun area -> fitness area, area)
                    |> Array.filter (accessFitness >> ((<)0.f<fit>))
                    |> Array.sortByDescending fst

                // Put this also here so the periodic backup works.
                x.Population <- alive

                // This is the exit condition (user closing window or pressing Ctrl+c).
                if pleaseStop.WaitOne 0 then alive else

                // Then we need lots of new ones for the next generation.
                simulate (generation + 1) lastTime ewma (breedPopulation alive)

            // Kickstart with some valid ones.
            match x.Population with
            | [||] -> laws.PopulationSize |> lengthRandomlyIncreasing |> simulate 0 DateTime.Now None
            | some -> some |> PArray.map (snd >> arrayToGenome) |> simulate 0 DateTime.Now None

        x.Population <- simulate()

    /// Start life simulation.
    member x.Simulate (pleaseStop:obj) = x.Simulate(pleaseStop :?> ManualResetEvent)

    /// Gets or sets the current scored population.
    member val Population : ((float32<percent> * float32<fit>) * LbpArea[])[] = [||] with get, set

    /// Writes to current population to a file on disk.
    member x.DumpToAsync(path) = x.DumpToAsync(path, false)

    /// Writes to current population to a file on disk.
    member __.DumpToAsync(_, _:bool) = async {
        printfn "Saving..."
    }
