Module Module1

    Sub Main()
        'debug()
        'test()

        Dim startTime As DateTime = Now
        Dim geneticTSP As New Algorithm(False, 20, 1, 1)
        geneticTSP.approximate(True, 1, 1)
        Dim stopTime As DateTime = Now
        Dim elapsedTime As TimeSpan = stopTime - startTime
        Console.WriteLine("Elapsed time: {0}ms", elapsedTime.Milliseconds)
        Console.ReadLine()
    End Sub
    Public Sub debug()
        Dim vertices = 8
        Dim tsp As New Tsp(False, vertices)
        Dim graph(,) As Int16 = tsp.getGraph

        Dim individual As New Individual(vertices)
        Dim genes() As Int16 = individual.getGenes()
        'Console.WriteLine(individual.getFitness(graph))
        Dim population As New Population(graph, 5000, vertices)
        Dim individuals = population.getPopulation
        Console.WriteLine(individuals(population.getBestIndividual).getFitness(graph))
        Console.WriteLine(population.getBestIndividual)
        Console.ReadLine()
    End Sub

    Public Sub test()
        Dim test As New Algorithm(True, 8, 1, 100)
        Dim individualA As New Individual(8)
        Dim individualB As New Individual(8)
        Dim individualC As New Individual(8)
        Dim genesC() As Int16
        For Each gene In individualA.getGenes()
            Console.Write(gene)
        Next
        Console.WriteLine()
        For Each gene In individualB.getGenes()
            Console.Write(gene)
        Next
        Console.WriteLine()

        For Each gene In test.getCrossOver(individualA.getGenes(), individualB.getGenes())
            Console.Write(gene)
        Next
        Console.WriteLine()
        genesC = individualC.getGenes()
        For Each gene In genesC
            Console.Write(gene)
        Next
        Console.WriteLine()
        test.mutate(genesC)
        For Each gene In genesC
            Console.Write(gene)
        Next

        Console.ReadLine()
    End Sub
    Class Tsp
        'creates an undirected weighted complete graph, or uses a preset graph
        Private graph(,) As Int16
        Public Sub New(usePreset As Boolean, vertices As Int16)
            If usePreset Then
                generatePreset(vertices)
            Else
                generateGraph(vertices)
            End If
        End Sub
        Private Sub generatePreset(vertices As Int16)
            Select Case vertices
                Case 4
                    ReDim graph(3, 3)
                    graph(0, 1) = 5
                    graph(0, 2) = 65
                    graph(0, 3) = 35
                    graph(1, 2) = 35
                    graph(1, 3) = 65
                    graph(2, 3) = 25
                    For i As Int16 = 0 To 3
                        For j As Int16 = 0 To 3
                            If i = j Then
                                graph(i, j) = 0
                            End If
                            graph(j, i) = graph(i, j)
                            'Console.Write(graph(i, j))
                            'Console.Write(" ")
                        Next
                        'Console.WriteLine()
                    Next
                Case 8
                    ReDim graph(7, 7)
                    graph(0, 1) = 10
                    graph(0, 2) = 40
                    graph(0, 3) = 17
                    graph(0, 4) = 21
                    graph(0, 5) = 19
                    graph(0, 6) = 18
                    graph(0, 7) = 15

                    graph(1, 2) = 11
                    graph(1, 3) = 32
                    graph(1, 4) = 19
                    graph(1, 5) = 19
                    graph(1, 6) = 19
                    graph(1, 7) = 18

                    graph(2, 3) = 12
                    graph(2, 4) = 19
                    graph(2, 5) = 20
                    graph(2, 6) = 19
                    graph(2, 7) = 17

                    graph(3, 4) = 12
                    graph(3, 5) = 19
                    graph(3, 6) = 19
                    graph(3, 7) = 20

                    graph(4, 5) = 13
                    graph(4, 6) = 26
                    graph(4, 7) = 19

                    graph(5, 6) = 13
                    graph(5, 7) = 22

                    graph(6, 7) = 14
                    For i As Int16 = 0 To 7
                        For j As Int16 = 0 To 7
                            If i = j Then
                                graph(i, j) = 0
                            End If
                            graph(j, i) = graph(i, j)
                        Next
                    Next
            End Select
        End Sub
        Private Sub generateGraph(vertices As Int16)
            ReDim graph(vertices - 1, vertices - 1)
            Dim weight As Int16
            For i = 0 To vertices - 1
                For j = 0 To vertices - 1
                    If i = j Then
                        graph(i, j) = 0
                    ElseIf graph(i, j) = 0 Then
                        weight = (100 + Rnd() * 100) / vertices
                        graph(i, j) = weight
                        graph(j, i) = weight
                        Randomize()
                    End If

                Next
            Next
        End Sub
        Public Function getGraph()
            Return graph
        End Function
    End Class
    Class Individual
        'individual solution candidates
        Private genes() As Int16

        Private vertices As Int16
        'Private rnd = New Random(Guid.NewGuid().GetHashCode())
        Private weightSum As Int32 = 0
        Private fitness As Int32 = 0

        Public Sub New(ByVal vertices As Int16)
            ReDim genes(vertices - 2)
            For i As Int16 = 0 To genes.Length - 1
                genes(i) = i + 1
            Next
            Shuffle(genes)
            Me.vertices = vertices
        End Sub
        Public Sub Shuffle(items As Int16())
            Dim j As Int32
            Dim temp As String
            For n As Int32 = items.Length - 1 To 0 Step -1
                'j = rnd.Next(0, n + 1)
                Randomize()
                j = Math.Floor(Rnd() * (n + 1))
                temp = items(n)
                items(n) = items(j)
                items(j) = temp
            Next n
        End Sub
        Public Function getGenes()
            Return genes
        End Function
        Public Function getFitness(graph(,) As Int16)
            'Fitness is the sum of all edge weights negated
            weightSum = 0
            weightSum += graph(0, genes(0))
            For i = 0 To vertices - 3
                weightSum += graph(genes(i), genes(i + 1))
            Next
            weightSum += graph(genes(vertices - 2), 0)
            fitness = -weightSum
            Return fitness
        End Function
        Public Sub alterGenes(genes() As Int16, graph(,) As Int16)
            Me.genes = genes
            Me.fitness = getFitness(graph)
        End Sub
    End Class
    Class Population
        'array of all individual candidates
        Private individuals() As Individual
        Private graph(,) As Int16
        Public Sub New(graph(,) As Int16, populationSize As Int16, vertices As Int16)
            ReDim individuals(populationSize - 1)
            For i = 0 To (populationSize - 1)
                Dim individual As New Individual(vertices)
                individuals(i) = individual
            Next
            Me.graph = graph
        End Sub
        Public Function getPopulation()
            Return individuals
        End Function
        Public Sub setPopulation(population() As Individual)
            individuals = population
        End Sub
        Public Function getBestIndividual()
            Dim bestFitness As Int16 = Int16.MinValue
            Dim bestIndividual As Int16
            Dim fitness As Int16
            For i = 0 To individuals.Length - 1
                fitness = individuals(i).getFitness(graph)
                If fitness > bestFitness Then
                    bestFitness = fitness
                    bestIndividual = i
                End If
            Next
            Return bestIndividual
        End Function
    End Class
    Class Algorithm
        'creates and evolves population with crossovers, mutations and survival of the fittest
        Private species As Population
        Private tsp As Tsp
        Private vertices As Int16
        Private population() As Individual

        Private mutationVariance As Single 'value between 0 and 1, where 0 = no mutations and 1 = always mutate 
        Private populationSize As Int16
        Private generation As Int16 = 0
        Public Sub New(usePreset As Boolean, vertices As Int16, mutationVariance As Single, populationSize As Int16)
            Me.vertices = vertices

            Me.mutationVariance = mutationVariance
            Me.populationSize = populationSize

            tsp = New Tsp(usePreset, vertices)
            species = New Population(tsp.getGraph, populationSize, vertices)
            population = species.getPopulation

        End Sub
        Public Sub approximate(showInfo As Boolean, everyXGenerations As Int16, totalGenerations As Int16)
            Do While generation < totalGenerations
                evolve()
                If generation Mod everyXGenerations = 0 And showInfo = True Then
                    Console.WriteLine("current generation: {0}", generation)
                    Console.Write("current best route:  0, ")
                    For Each gene In population(species.getBestIndividual).getGenes
                        Console.Write(gene)
                        Console.Write(", ")
                    Next
                    Console.WriteLine("0")
                    Console.WriteLine("fitness: {0}", population(species.getBestIndividual).getFitness(tsp.getGraph))
                    Console.WriteLine()
                End If
            Loop
            Console.Write("best route after {0} generations:  0, ", generation)
            For Each gene In population(species.getBestIndividual).getGenes
                Console.Write(gene)
                Console.Write(", ")
            Next
            Console.WriteLine("0")
            Console.WriteLine("fitness: {0}", population(species.getBestIndividual).getFitness(tsp.getGraph))

        End Sub
        Private Sub evolve() 'Evolves current generation to next generation
            Dim newPopulation(population.Length - 1) As Individual
            newPopulation(0) = New Individual(vertices)
            newPopulation(0).alterGenes(population(species.getBestIndividual).getGenes, tsp.getGraph)
            For i = 1 To population.Length - 1
                newPopulation(i) = New Individual(vertices)
                newPopulation(i).alterGenes(getCrossOver(population(i - 1).getGenes, population(i).getGenes), tsp.getGraph)
                mutate(newPopulation(i).getGenes)
                mutate(newPopulation(i).getGenes)
                mutate(newPopulation(i).getGenes)
                mutate(newPopulation(i).getGenes)
                mutate(newPopulation(i).getGenes)
            Next
            generation += 1
            population = newPopulation
            species.setPopulation(population)
        End Sub
        Public Sub mutate(ByRef genes() As Int16) 'shuffle two chromosomes. Likeliness dependent on mutationVariance
            Dim a As Int16
            Dim b As Int16
            Randomize()
            Dim c As Single = Rnd()
            Dim rndIndexA As Int16
            Dim rndIndexB As Int16
            If c < mutationVariance Then
                Randomize()
                rndIndexA = Math.Floor(Rnd() * (vertices - 1))
                Randomize()
                rndIndexB = Math.Floor(Rnd() * (vertices - 1))
                a = genes(rndIndexA)
                b = genes(rndIndexB)
                genes(rndIndexA) = b
                genes(rndIndexB) = a
            End If
        End Sub
        Public Function getCrossOver(genesA() As Int16, genesB() As Int16) 'maintain route pattern from half of genesA, fill in rest as required from genesB, maintaing order
            Dim newGenes(vertices - 2) As Int16

            Dim half As Int16 = Math.Floor((vertices - 2) / 2)
            Dim subString(half) As String

            Dim bClone = genesB.Clone
            bClone = Array.ConvertAll(Of Int16, String)(bClone, Function(x) x.ToString())

            For i = 0 To (half)
                subString(i) = genesA(i).ToString
            Next
            For Each gene In subString
                bClone(Array.IndexOf(bClone, gene)) = ""
            Next
            Dim j As Int16 = 0
            For Each gene In bClone
                If gene <> "" Then
                    newGenes(j) = gene
                    j += 1
                End If
            Next
            For k = 0 To half
                newGenes(j) = subString(k)
                j += 1
            Next
            Return newGenes
        End Function
        Public Function getGeneration()
            Return generation
        End Function
    End Class
End Module