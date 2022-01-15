PennController.ResetPrefix(null) // Keep here
PennController.DebugOff()

var progressBarText = "Quanto ainda falta?"

// Gera um código identificador único para cada participante
code = (()=>([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g,a=>(a^Math.random()*16>>a/4).toString(16)))()

Sequence("apresent", "tcle", "form", "instru", "trein", "FimDoTreino", rshuffle("experiment", "distrat"), SendResults() , "fim")

// Adiciona o código identificador do participante aos resultados
Header()
    .log("ID", code)
    ,
//-------------------------------------------- APRESENTAÇÃO ----------------------------------------
newTrial("apresent",

    newHtml("instrucoes", "Instrucoes.html")
        .print()
    
    ,
    newButton("meubotao", "Quero ler o Termo de Consentimento!")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print()
        .wait()
    )
    ,

//---------------------------------------------- TCLE --------------------------------------------
newTrial("tcle",
    newHtml("meutcle", "tcle.html")
        .checkboxWarning("Você deve dar seu consentimento para participar da pesquisa.")
        .print()
    
    ,
    newButton("CONTINUAR")
    .css("font-size", "20px")
    .cssContainer({"margin-bottom":"3em"})
    .center()
    .print()
    .wait(
            getHtml("meutcle").test.complete()
                .failure(getHtml("meutcle").warn())
        )
)
,

//-------------------------------- Formulário de dados pessoais ----------------------------------
newTrial("form",
    newText("<b>Por favor, preencha alguns dados pessoais:</b><br><br>")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    // ------------- Idade ------------------------
    newText("<br>Idade (apenas números):")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    newTextInput("idade", "")
        .center()
        .css("margin","1em")
        .print()
        .length(2)
    ,
    
    // ------------- Gênero ------------------------
    newText("Gênero:")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    newDropDown("genero", "selecionar")
        .css("font-size", "20px")
        .css("margin","1em")
        .add("Feminino", "Masculino", "Outro", "Prefiro não informar")
        .center()
        .print()
    ,
    
    // ------------- Escolaridade ------------------------
    newText("Escolaridade:")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    newDropDown("escolaridade", "selecionar")
        .css("font-size", "20px")
        .center()
        .add("Ensino Fundamental incompleto",
                "Ensino Fundamental completo",
                "Ensino Médio incompleto",
                "Ensino Médio completo",
                "Ensino Superior incompleto",
                "Ensino Superior completo",
                "Pós-Graduação")
        .center()
        .css("margin","1em")
        .print()
    ,
    
    // ------------- Nativo ------------------------
    newText("Você é falante nativo de português brasileiro?")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    newDropDown("nativo", "selecionar")
        .css("font-size", "20px")
        .center()
        .add("Sim", "Não")
        .center()
        .css("margin","1em")
        .print()
    ,
    
    // ------------- Botão para formulário ------------------------
    newButton("CONTINUAR")
        .css("font-size", "20px")
        .center()
        .print()
        .wait(
            getTextInput("idade").test.text(/^\d+$/)
                .and(
                    getDropDown("genero").test.selected())
                        .and(
                            getDropDown("escolaridade").test.selected())
                                .and(
                                    getDropDown("nativo").test.selected())
                                        .failure(
                                            newText('erro_preench', "Todos os campos são obrigatórios.")
                                                .css("color", "red")
                                                .print()
                                                .center()
                                                )
            )
    ,
    
    // ------------- Variáveis a serem acessadas fora desse trial ------------------------
     newVar("IDADE")
        .global()
        .set(getTextInput("idade"))
    ,
    
     newVar("GENERO")
        .global()
        .set(getDropDown("genero"))
    , 
    
    newVar("ESCOLARIDADE")
        .global()
        .set(getDropDown("escolaridade"))
    ,
    
    newVar("NATIVO")
        .global()
        .set(getDropDown("nativo"))
    
)
,

//-------------------------------------------- INSTRUÇÕES ----------------------------------------
newTrial("instru",

    newHtml("instrucoes", "Instrucoes2.html")
        .print()
    
    ,
    newButton("meubotao", "Agora sim, vamos ao treino!")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print()
        .wait()
)
,

// --------------------------------- TREINO ---------------------------------
Template("Treino.csv", trn =>
    newTrial("trein",

//----------------------------------------------------------------------------------------    
// ETAPA 1 - Leitura automonitorada da sentença
//----------------------------------------------------------------------------------------    
     newController("DashedSentence", {s: trn.frase})
            .css({"white-space": "nowrap", 'font-size':"25px",'font-family': "Calibri", "height": "30px", 'margin':"2em" })
            .center()
            .print()
            .log()
            .wait()
            .remove()
    ,
//----------------------------------------------------------------------------------------    
// ETAPA 2 - Pergunta de interpretação da sentença
//----------------------------------------------------------------------------------------    
    newText("pergunta", trn.pergunta)
        .cssContainer({"font-size": "125%", 'margin':"1em"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.esgotado", 5000)
        .start()
    ,
    
    newScale("resposta", trn.respA, trn.respB)
        .cssContainer({"font-size": "125%", "line-height": "125%"})
        .radio()
        .labelsPosition("right")
        .vertical()
        .print("center at 50%", "bottom at 36%")
        .callback(getTimer("tempo.esgotado").stop())
        .log()
    ,
    
    getTimer("tempo.esgotado")
        .wait()
    ,
    
    clear()
    ,
    
    getScale("resposta")
        .test.selected()
        .success()
        .failure(
            newAudio("falha", "Error.mp3")
                .play()
            ,
            newText("Seja mais rápido!")
                .cssContainer({"font-size": "150%", "color": "red"})
                .print("center at 50%", "bottom at 32%")
                .center()
            ,
            
            getAudio("falha")
                .wait()
        )
    ,
    
    clear()
    ,
    
    // Botão final ativado pelo click do mouse ou pela barra de espaço
    
    newKey(" ") // tecla barra de espaço
        .callback(
            getButton("validation")
                .click())
        ,
    
    newButton("validation", "Próxima")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print("center at 50%", "bottom at 32%")
        .wait()
    
    ) // Fechamento do newTrial
    .log("vies", trn.vies)
    .log("item", trn.item)
    .log("frase", trn.frase)
    .log("lista", trn.item)
    .log("idade", getVar("IDADE"))
    .log("genero", getVar("GENERO"))
    .log("escolaridade", getVar("ESCOLARIDADE"))
    .log("nativo", getVar("NATIVO"))
) // Fechamento do Template

,

newTrial("FimDoTreino",
    defaultText
        .cssContainer({"font-size": "125%"})
        .center()
        .print()
    ,

    newText("O treino acabou!")
    ,
    
    newText("<br>Vamos começar?<br><br>")
    ,

    newButton("meubotao", "Sim!")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print()
        .wait()
)

,


// --------------------------------- EXPERIMENTAIS ---------------------------------
Template("Experimentais.csv", exp =>
    newTrial("experiment",

//----------------------------------------------------------------------------------------    
// ETAPA 1 - Apresentação da sentença
//----------------------------------------------------------------------------------------    
    newController("DashedSentence", {s: exp.frase})
            .css({"white-space": "nowrap", 'font-size':"25px",'font-family': "Calibri", "height": "30px", 'margin':"2em" })
            .center()
            .print()
            .log()
            .wait()
            .remove()
    ,
//----------------------------------------------------------------------------------------    
// ETAPA 2 - Pergunta de interpretação da sentença
//----------------------------------------------------------------------------------------    
    newText("pergunta", exp.pergunta)
        .cssContainer({"font-size": "125%", 'margin':"1em"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.esgotado", 5000)
        .start()
    ,
    
    newScale("resposta", exp.respA, exp.respB)
        .cssContainer({"font-size": "125%", "line-height": "125%"})
        .radio()
        .labelsPosition("right")
        .vertical()
        .print("center at 50%", "bottom at 36%")
        .callback(getTimer("tempo.esgotado").stop())
        .log()
    ,
    
    getTimer("tempo.esgotado")
        .wait()
    ,
    
    clear()
    ,
    
    getScale("resposta")
        .test.selected()
        .success()
        .failure(
            newAudio("falha", "Error.mp3")
                .play()
            ,
            newText("Seja mais rápido!")
                .cssContainer({"font-size": "150%", "color": "red"})
                .print("center at 50%", "bottom at 32%")
                .center()
            ,
            getAudio("falha")
                .wait()
        )
    ,
    
    clear()
    ,
    
    // Botão final ativado pelo click do mouse ou pela barra de espaço
     
    newKey(" ") // tecla barra de espaço
        .callback(
            getButton("validation")
                .click())
        ,
    
    newButton("validation", "Próxima")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print("center at 50%", "bottom at 32%")
        .wait()
    )
    .log("vies", exp.vies)
    .log("item", exp.item)
    .log("frase", exp.frase)
    .log("lista", exp.group)
    .log("idade", getVar("IDADE"))
    .log("genero", getVar("GENERO"))
    .log("escolaridade", getVar("ESCOLARIDADE"))
    .log("nativo", getVar("NATIVO"))
)
,
// --------------------------------- DISTRATORES ---------------------------------
Template("Distratores.csv", dist =>
    newTrial("distrat",

//----------------------------------------------------------------------------------------    
// ETAPA 1 - Apresentação da sentença
//----------------------------------------------------------------------------------------    
    newController("DashedSentence", {s: dist.frase})
            .css({"white-space": "nowrap", 'font-size':"25px",'font-family': "Calibri", "height": "30px", 'margin':"2em" })
            .center()
            .print()
            .log()
            .wait()
            .remove()
    ,
//----------------------------------------------------------------------------------------    
// ETAPA 2 - Pergunta de interpretação da sentença
//----------------------------------------------------------------------------------------    
    newText("pergunta", dist.pergunta)
        .cssContainer({"font-size": "125%", 'margin':"1em"})
        .print("center at 50%", "bottom at 25%")
    ,
    
    newTimer("tempo.esgotado", 5000)
        .start()
    ,
    
    newScale("resposta", dist.respA, dist.respB)
        .cssContainer({"font-size": "125%", "line-height": "125%"})
        .radio()
        .labelsPosition("right")
        .vertical()
        .print("center at 50%", "bottom at 36%")
        .callback(getTimer("tempo.esgotado").stop())
        .log()
    ,
    
    getTimer("tempo.esgotado")
        .wait()
    ,
    
    clear()
    ,
    
    getScale("resposta")
        .test.selected()
        .success()
        .failure(
            newAudio("falha", "Error.mp3")
                .play()
            ,
            newText("Seja mais rápido!")
                .cssContainer({"font-size": "150%", "color": "red"})
                .print("center at 50%", "bottom at 32%")
                .center()
            ,
            getAudio("falha")
                .wait()
        )
    ,
    
    clear()
    ,
    
    // Botão final ativado pelo click do mouse ou pela barra de espaço
    
    newKey(" ") // tecla barra de espaço
        .callback(
            getButton("validation")
                .click())
        ,
    
    newButton("validation", "Próxima")
        .css("margin","1em")
        .css("font-size", "20px")
        .center()
        .print("center at 50%", "bottom at 32%")
        .wait()
)
    .log("vies", dist.vies)
    .log("item", dist.item)
    .log("frase", dist.frase)
    .log("lista", dist.posicao)
    .log("idade", getVar("IDADE"))
    .log("genero", getVar("GENERO"))
    .log("escolaridade", getVar("ESCOLARIDADE"))
    .log("nativo", getVar("NATIVO"))
)
,

//-------------------------------------------- TELA FINAL ----------------------------------------

newTrial("fim",
    newText("Acabamos! Obrigado pela colaboração!<br><br>")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    newText("Se quiser saber mais sobre essa pesquisa, entre em contato pelo e-mail <a href='mailto:igordeo.costa@gmail.com' target='_blank'>igordeo.costa@gmail.com</a> ou visite <a href='https://igordeo-costa.github.io/' target='_blank'>o blog do pesquisador</a>.<br><br>")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    newText("Visite, também, o site do <a href='http://www.lapal.letras.puc-rio.br/' target='_blank'>LAPAL/PUC-Rio</a>.<br><br>")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    newText("Se quiser receber um certificado de participação nesta tarefa, copie o código identificador abaixo e envie-o, com seu nome completo (e número de matrícula, caso seja universitário), para o e-mail disponível acima.<br><br>")
        .cssContainer({"font-size": "125%"})
        .print()
        .center()
    ,
    
    newText(code)
        .cssContainer({"font-size": "125%"})
        .bold()
        .center()
        .print()
    ,
    
    newButton("Sair do experimento")
        .wait()
    )
    code = undefined // Make it harder for participants to fetch the code early on