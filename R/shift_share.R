#' A Shift-Share Function
#'
#' This function allows you to do shift-share analysis on rates (e.g. employment rate, basketball field goal % for 2 point - 3 point analysis)
#' @param ap_grp_labels #groups
#' @param ap_numerator1 #numerator of rate 1
#' @param ap_numerator2 #numerator of rate 2
#' @param ap_denominator1 #denominator of rate 1
#' @param ap_denominator2 #denominator of rate 2
#' @return object

shift_share <- function(ap_grp_labels, ap_numerator1, ap_numerator2, ap_denominator1, ap_denominator2){

  ## Get the environment for this
  thisEnv <- environment()

  #Initializing parameters here
  ap_grp_labels = ap_grp_labels

  ap_numerator1 = ap_numerator1
  ap_numerator2 = ap_numerator2

  ap_denominator1 = ap_denominator1
  ap_denominator2 = ap_denominator2

  prop1 = NULL
  prop2 = NULL
  rate1 = NULL
  rate2 = NULL

  agg_effect_topdown = NULL
  agg_effect_btmup = NULL

  within_effect = NULL
  across_effect = NULL
  dynamic_effect = NULL

  all_effects = NULL

  ## Create the list used to represent an
  ## object for this class
  methd <- list(

    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,

    ## Define the accessors for the data fields.
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    },

    getGrp_labels = function()
    {
      return(get("ap_grp_labels",thisEnv))
    },

    getNumerator1 = function()
    {
      return(get("ap_numerator1",thisEnv))
    },

    getNumerator2 = function()
    {
      return(get("ap_numerator2",thisEnv))
    },

    getDenominator1 = function()
    {
      return(get("ap_denominator1",thisEnv))
    },

    getDenominator2 = function()
    {
      return(get("ap_denominator2",thisEnv))
    },

    setNumerator1 = function(value)
    {
      return(assign("ap_numerator1",value,thisEnv))
    },

    setNumerator2 = function(value)
    {
      return(assign("ap_numerator2",value,thisEnv))
    },

    setDenominator1 = function(value)
    {
      return(assign("ap_denominator1",value,thisEnv))
    },

    setDenominator2 = function(value)
    {
      return(assign("ap_denominator2",value,thisEnv))
    },

    setProp1 = function()
    {
      prop1 = ap_denominator1 / sum(ap_denominator1)
      print(ap_denominator1)
      return(assign("prop1",prop1,thisEnv))
    },

    setProp2 = function()
    {
      prop2 = ap_denominator2 / sum(ap_denominator2)
      return(assign("prop2", prop2, thisEnv))
    },

    setRate1 = function()
    {
      rate1 = ap_numerator1 / ap_denominator1
      return(assign("rate1", rate1, thisEnv))
    },

    setRate2 = function()
    {
      rate2 = ap_numerator2 / ap_denominator2
      return(assign("rate2", rate2,thisEnv))
    },

    getWithin_effect = function()
    {
      return(get("within_effect",thisEnv))
    },

    getAcross_effect = function()
    {
      return(get("across_effect",thisEnv))
    },

    getDynamic_effect = function()
    {
      return(get("dynamic_effect",thisEnv))
    },

    setWithin_effect = function()
    {
      methd$setProp1()
      methd$setRate2()
      methd$setRate1()

      within_effect = prop1 * (rate2 - rate1)

      return(assign("within_effect", within_effect,thisEnv))
    },

    setAcross_effect = function()
    {
      methd$setProp1()
      methd$setProp2()
      methd$setRate1()

      print(rate1)
      print(prop2)
      print(prop1)

      across_effect = rate1 * (prop2 - prop1)

      return(assign("across_effect", across_effect, thisEnv))
    },

    setDynamic_effect = function()
    {
      methd$setProp1()
      methd$setProp2()
      methd$setRate1()
      methd$setRate2()

      dynamic_effect = (rate2 - rate1) * (prop2 - prop1)
      # print(paste("Hello", dynamic_effect))
      return(assign("dynamic_effect", dynamic_effect, thisEnv))
    },

    setAgg_effect_topdown = function()
    {
      agg_effect_topdown = sum(ap_numerator2)/ sum(ap_denominator2) - sum(ap_numerator1)/ sum(ap_denominator1)
      return(assign("agg_effect_topdown", agg_effect_topdown, thisEnv))
    },

    setAgg_effect_btmup_detailed = function()
    {
      methd$setWithin_effect()
      methd$setAcross_effect()
      methd$setDynamic_effect()

      agg_effect_btmup = within_effect + across_effect + dynamic_effect

      return(assign("agg_effect_btmup", agg_effect_btmup, thisEnv))
    },

    get_effects = function(){

      methd$setAgg_effect_btmup_detailed()

      all_effects = data.frame(
                               grp_labels =  methd$getGrp_labels(),
                               prop1 = prop1,
                               prop2 = prop2,
                               rate1 = rate1,
                               rate2 = rate2,
                               within_effect = within_effect,
                               across_effect = across_effect,
                               dynamic_effect = methd$getDynamic_effect(),
                               overall_effect = agg_effect_btmup
                               )
      assign("all_effects", all_effects, thisEnv)
      return(get("all_effects", all_effects, thisEnv))
    }

  )

  ## Define the value of the list within the current environment.
  assign('this', shift_share, envir = thisEnv)

  ## Set the name for the class
  class(methd) <- append(class(methd),"shift_share")
  return(methd)
}


###########################Demonstration of OOP version for shift-share analysis#######################
# emp1 = c(10, 20, 40, 50)
# pop1 = c(40, 50, 60, 70)
# emp2 = c(20, 30, 50, 60)
# pop2 = c(50, 70, 20, 50)
#
# ss_analysis <- shift_share(ap_grp_labels = c("grp1", "grp2", "grp3", "grp4"),
#                            ap_numerator1 = emp1,
#                            ap_numerator2 = emp2,
#                            ap_denominator1 = pop1,
#                            ap_denominator2 = pop2)
#
#
# ss_analysis$setAcross_effect()
# ss_analysis$getAcross_effect()
#
# ss_analysis$setDynamic_effect()
# ss_analysis$getDynamic_effect()
#
# apply(ss_analysis$get_effects()[,-1], 2, sum)
#########################################################################################################

# #Non OOP way for shift share analysis
# emp1 = c(10, 20, 40, 50)
# pop1 = c(40, 50, 60, 70)
# emp_rate1 = emp1 / pop1
# pop_prop1 = pop1 / sum(pop1)
#
# emp2 = c(20, 30, 50, 60)
# pop2 = c(50, 70, 20, 50)
# emp_rate2 = emp2 / pop2
# pop_prop2 = pop2 / sum(pop2)
#
# emp_agg = sum(emp2) - sum(emp1)
# emp_rate_agg = sum(emp2)/sum(pop2) - sum(emp1)/ sum(pop1)
# emp = emp2 - emp1
# emp_rate = emp_rate2 - emp_rate1
#
# pop_prop = pop_prop2 - pop_prop1
#
# within = pop_prop1 * emp_rate
# across = emp_rate1 * pop_prop
# dynamic = emp_rate * pop_prop
#
# sum(within) + sum(across) + sum(dynamic)





