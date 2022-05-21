namespace SampleFsPredictor


type CancellationTokenSource = System.Threading.CancellationTokenSource

type Deferred<'t> =
    | HasNotStartedYet
    | InProgress of CancellationTokenSource
    | Resolved of 't

type Msg<'t> =
    | Set of Deferred<'t>
    | Get of AsyncReplyChannel<'t option>

type Actor<'t>() =
    let agent =
        fun (inbox: MailboxProcessor<Msg<'t>>) ->
            let rec loop state =
                async {
                    let! msg = inbox.Receive()

                    let newState =
                        match msg, state with
                        | Get ch, Resolved t ->
                            Some t |> ch.Reply
                            state
                        | Get ch, _ ->
                            None |> ch.Reply
                            state

                        // HasNotStartedYetで初期化
                        | Set HasNotStartedYet, InProgress cts ->
                            cts.Cancel()
                            HasNotStartedYet
                        | Set HasNotStartedYet, Resolved _ -> HasNotStartedYet

                        // 開始処理
                        | Set (InProgress _ as newState), HasNotStartedYet -> newState
                        // InProgressならCancelして開始処理
                        | Set (InProgress _ as newState), InProgress cts ->
                            cts.Cancel()
                            newState
                        // 完了したら結果をセット
                        | Set (Resolved _ as newState), HasNotStartedYet
                        | Set (Resolved _ as newState), InProgress _ -> newState
                        // これ以外は無視
                        | _ -> state

                    return! loop newState
                }

            loop HasNotStartedYet
        |> MailboxProcessor.Start

    member _.Start job =
        let cts = new CancellationTokenSource()
        InProgress cts |> Set |> agent.Post

        let token = cts.Token

        task {
            token.ThrowIfCancellationRequested()
            let! result = job
            Resolved result |> Set |> agent.Post
        }


    member _.TryGerResult() = agent.PostAndReply Get

    member _.Stop() = Set HasNotStartedYet |> agent.Post

module IDisposable =
    open System

    let create onDispose =
        { new IDisposable with
            member _.Dispose() = onDispose () }

module NugetAutoComplate =
    open System.Net.Http
    open System.Net.Http.Json

    type Response = { totalHits: int; data: string [] }

    let private client = new HttpClient()

    let getAsync take prerelease q =
        task {
            return!
                $"https://api-v2v3search-0.nuget.org/autocomplete?q=%s{q}&prerelease=%b{prerelease}&take=%i{take}"
                |> client.GetFromJsonAsync<Response>
        }


module SampleFsPredictor =
    open System
    open System.Management.Automation
    open System.Management.Automation.Subsystem
    open System.Management.Automation.Subsystem.Prediction
    open Microsoft.PowerShell

    let mutable isActive = false
    let private guid = Guid "1724e34c-89ce-43fd-b50f-fc5335bde7d4"

    type SampleFsPredictor() =
        let actor = Actor()

        let getAutocompletes q =
            task {
                let! r = NugetAutoComplate.getAsync 10 false q

                return
                    ResizeArray [| for data in r.data do
                                       PredictiveSuggestion data |]
                    |> SuggestionPackage
            }

        /// PredictionSourceを変更する。
        let setPredictionSource predictionSource =
            SetPSReadLineOption(PredictionSource = predictionSource)
            |> PSConsoleReadLine.SetOptions

        // TODO: 適切なPrediction更新を実現するAPIを見つけたら書き換える。
        /// Predictionを更新する。
        let reloadPrediction () =
            task {
                let prev = PSConsoleReadLine.GetOptions().PredictionSource
                setPredictionSource PredictionSource.None
                use _ = IDisposable.create (fun _ -> setPredictionSource prev)
                PSConsoleReadLine.Insert " "
                use _ = IDisposable.create PSConsoleReadLine.Undo
                setPredictionSource prev
            }

        interface ICommandPredictor with
            /// サブシステム実装の一意な識別子を取得する。
            member _.Id = guid

            /// サブシステム実装の名前を取得する。
            member _.Name = nameof SampleFsPredictor

            /// サブシステム説明を取得する。
            member _.Description = "A sample F# delay predictor"

            member _.GetSuggestion(client, context, cancellationToken) =

                match actor.TryGerResult() with
                | Some suggest -> suggest
                | None ->
                    let input = context.InputAst.Extent.Text

                    task {
                        do! getAutocompletes input |> actor.Start
                        do! reloadPrediction ()
                        actor.Stop()
                    }
                    |> ignore

                    Unchecked.defaultof<_>


            member _.CanAcceptFeedback(client, feedback) =


                match feedback with
                | PredictorFeedbackKind.SuggestionDisplayed
                | PredictorFeedbackKind.SuggestionAccepted
                | PredictorFeedbackKind.CommandLineAccepted
                | PredictorFeedbackKind.CommandLineExecuted -> false
                | _ -> false

            member _.OnSuggestionDisplayed(client, session, countOrIndex) =
                // Console.Write "OnSuggestionDisplayed"
                ()

            member _.OnSuggestionAccepted(client, session, acceptedSuggestion) =
                // Console.Write "OnSuggestionAccepted"
                ()

            member _.OnCommandLineAccepted(client, history) =
                // Console.Write "OnCommandLineAccepted"
                ()

            member _.OnCommandLineExecuted(client, commandLine, success) =
                // Console.Write "OnCommandLineExecuted"
                ()


    type Init() =

        interface IModuleAssemblyInitializer with
            member _.OnImport() =
                SampleFsPredictor()
                |> SubsystemManager.RegisterSubsystem<ICommandPredictor, _>

        interface IModuleAssemblyCleanup with
            member _.OnRemove(psModuleInfo) =
                guid
                |> SubsystemManager.UnregisterSubsystem<ICommandPredictor>
