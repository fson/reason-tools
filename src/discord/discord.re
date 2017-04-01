type client;

type message;

type user;

type channel;

type codeBlock;

external createClient : unit => client = "Client" [@@bs.module "discord.js"] [@@bs.new];

external login : client => string => unit = "" [@@bs.send];

external onReady : client => _ [@bs.as "ready"] => (unit => unit) => unit = "on" [@@bs.send];

external onMessage : client => _ [@bs.as "message"] => (message => unit) => unit =
  "on" [@@bs.send];

external getContent : message => string = "content" [@@bs.get];

external getChannel : message => channel = "channel" [@@bs.get];

external getType : channel => string = "type" [@@bs.get];

external getAuthor : message => option user =
  "author" [@@bs.get] [@@bs.return null_undefined_to_opt];

external isBot : user => bool = "bot" [@@bs.get];

external reply : message => string => unit = "" [@@bs.send];

external codeBlocks : string => array codeBlock = "gfm-code-blocks" [@@bs.module];

external getCode : codeBlock => string = "code" [@@bs.get];

let discordClient = createClient ();

let syntaxLang lang =>
  switch lang {
  | "ML" => "ocaml"
  | "RE"
  | _ => "rust"
  };

let humanLang lang =>
  switch lang {
  | "ML" => "OCaml"
  | "RE"
  | _ => "Reason"
  };

let contains s1 s2 =>
  try {
    let len = String.length s2;
    for i in 0 to (String.length s1 - len) {
      if (String.sub s1 i len == s2) {
        raise Exit
      }
    };
    false
  } {
  | Exit => true
  };

let getCodeBlocks content => content |> codeBlocks |> Js.Array.map getCode;

let hasRefmt content => contains content "refmt";

let shouldReply message =>
  switch (message |> getContent |> hasRefmt, getAuthor message, message |> getChannel |> getType) {
  | (false, Some author, "dm")
  | (true, Some author, _) when not (isBot author) => true
  | _ => false
  };

let handleReady _ => Js.log "ready!";

let break = "\n";

let codifyString code lang => "\n```" ^ lang ^ "\n" ^ code ^ "\n" ^ "```";

let languageString inLang outLang =>
  "here is what your " ^ humanLang inLang ^ " code would look like in " ^ humanLang outLang ^ ":";

let errorString = "It looks like you asked for me, but I can't find any valid OCaml or Reason code in this message. :(";

let refmtCodeBlock codeBlock => codeBlock |> RefmtBS.refmt |> RefmtBS.parse;

let singleResultString (res: Protocol.Refmt.payload) =>
  languageString res.inLang res.outLang ^ codifyString res.outText (syntaxLang res.outLang);

let listFilterMap ls => {
  let rec listFilterMap_ ls acc =>
    switch ls {
    | [] => acc
    | [item, ...rest] =>
      switch item {
      | Rebase.Ok item => [item, ...listFilterMap_ rest acc]
      | _ => acc
      }
    };
  listFilterMap_ ls []
};

let handleMessage message => {
  Js.log message;
  let content = getContent message;
  let codeBlocks = getCodeBlocks content;
  if (shouldReply message) {
    let results = codeBlocks |> Array.to_list |> List.map refmtCodeBlock |> listFilterMap;
    switch (List.length results) {
    | 0 => reply message errorString
    | _ => reply message (List.map singleResultString results |> String.concat "\n")
    }
  }
};

onReady discordClient handleReady;

onMessage discordClient handleMessage;

login discordClient "";

Js.log "trying this thing";
