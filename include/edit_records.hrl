-type document_id() :: string().
-type diff() :: itweet_mochijson2:json_object().

-record(edit_user, {id        :: pos_integer(),
                    username  :: string()}).
-record(edit_document, {id              :: document_id(),
                        title = <<"">>  :: binary(),
                        body = <<"">>   :: binary(),
                        users = []      :: [#edit_user{}],
                        hash_tags = []  :: [binary()]}).
-record(edit_dv, {document :: document_id(),
                  version  :: non_neg_integer(),
                  patch    :: iodata(),
                  user     :: #edit_user{}}).