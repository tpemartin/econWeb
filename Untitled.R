


ui() |> browseTag2()

#needs to correct inside-autolayout width/height constraints

input[[1]]()
purrr::map(
  seq_along(split_div),
  ~{
    function() do.call(split_div[[.x]], input[[.x]]())
  }
) -> split


pc_map[[1]]
cssnames[pc_map[[1]]]

cssnames[whichBelong2X]
requireFlexOrder = 0L
.currentAL=1
xpool <- (.currentAL+1):length(split_css)
.x =xpool[[1]]
.x
flag_autolayout <-
  !(is.null(split_css[[.x]]$auto_layout))
if(flag_autolayout){
  .currentAL=.x
}
for(y in 1:3){
  print(y)
  for(x in 1:3){

    if(y==2 && x==2) break
    print(x)
  }
}

update_css_belong <- function(split_css, requireFlexOrder, css_belong, .currentAL, flag_autolayout) {}
.currentAL=0
for(.x in seq_along(split_css)){
  {
    flag_autolayout <-
      !(is.null(split_css[[.x]]$auto_layout))
    flag_insideAL <-
      !(is.null(split_css[[.x]]$inside_autoLayout))
    flag_tagOrderMatch <- {
      split_css[[.x]]$inside_autoLayout$order |> as.integer() ->
        tagOrder
      tagOrder==requireFlexOrder
    }

    if(flag_insideAL){
      if(flag_tagOrderMatch) {
        css_belong[[.x]]=.currentAL
        if(flag_autolayout){
          .currentAL=.x
          requireFlexOrder=0L
        } else {
          requireFlexOrder=requireFlexOrder+1L
        }
      } else if(!flag_tagOrderMatch){
        css_belong[[.x]]=css_belong[[.currentAL]]
        .currentAL = css_belong[[.currentAL]]
      }
    } else {

    }
  }

  if(flag_autolayout) {
    .currentAL=.x
    next
  }
  if()

}
xpool <- (.currentAL+1):length(split_css)
for(.x in xpool)

# 1 -2,3
# 3 - 4,5
input <- vector("list", 5)
input[1:5]=list(function(){NULL})
list_dep = list(
  c(2,3),
  NULL,
  c(4,5),
  NULL,
  NULL
)
list_dep |> purrr::map(
  ~get_divInputFunction(.x, split_div)
) -> input
debug(input[[1]])
split_div[[1]](input[[1]]())
input[[1]]()

