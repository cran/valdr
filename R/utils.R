#' Safely extract a field from a list of lists
#'
#' Internal helper function used to extract a specified field from a list of records.
#' If a field value is \code{NULL}, it is replaced with an empty string (\code{""}).
#'
#' @param x A list of lists (records), coming from parsed JSON API responses.
#' @param field A character string naming the field to extract from each record.
#'
#' @return A character vector containing the extracted values, with \code{NULL}s replaced by \code{""}.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.safe_extract <- function(x, field) {
    v <- sapply(x, function(el) {
        val <- el[[field]]
        if (is.null(val)) "" else val
    }, USE.NAMES = FALSE)
    return(v)
}

#' Safely extract the first non-empty field from a list of lists
#'
#' Internal helper function used to extract the first non-empty value
#' from a list of fields, in order, from a list of records.
#' If all fields are \code{NULL} or empty strings, returns \code{""}.
#'
#' @param x A list of lists (records), coming from parsed JSON API responses.
#' @param fields A character vector naming the candidate fields to check in order.
#'
#' @return A character vector containing the extracted values, with \code{NULL}s replaced by \code{""}.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.safe_extract_first <- function(x, fields) {
    v <- sapply(x, function(el) {
        for (f in fields) {
            val <- el[[f]]
            if (!is.null(val) && nzchar(trimws(val))) {
                return(val)
            }
        }
        ""
    }, USE.NAMES = FALSE)
    return(v)
}

#' Safely extract a single value
#'
#' Internal helper function used to safely retrieve a single value from an object.
#' If the value is \code{NULL}, it is replaced with an empty string (\code{""}).
#'
#' @param x A single value, possibly \code{NULL}.
#'
#' @return The original value, or \code{""} if \code{NULL}.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.safe_value <- function(x) {
    if (is.null(x)) {
        return("")
    }
    return(x)
}

#' Validate a single GUID test ID
#'
#' Internal helper to validate that exactly one non-empty, non-NA,
#' string-based GUID has been supplied.
#'
#' @param ... One or more values for the test ID.
#' @param arg_name Optional string for the argument name to use in error messages.
#'
#' @return A single validated test ID string.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.validate_single_guid <- function(..., arg_name = "`test_id`") {
    # Capture all arguments into a single vector
    test_id <- unlist(list(...), use.names = FALSE)

    if (length(test_id) == 0L) {
        stop(arg_name, " must be provided.", call. = FALSE)
    }
    if (length(test_id) != 1L) {
        stop(
            "You can only pass 1 ", arg_name, " to this function at a time. ",
            "You supplied ", length(test_id), ".",
            call. = FALSE
        )
    }
    if (is.null(test_id) || is.na(test_id)) {
        stop(arg_name, " cannot be NULL or NA.", call. = FALSE)
    }
    if (!is.character(test_id)) {
        stop(arg_name, " must be a character string (length 1).", call. = FALSE)
    }
    test_id <- trimws(test_id)
    if (!nzchar(test_id)) {
        stop(arg_name, " cannot be an empty string.", call. = FALSE)
    }

    # GUID format validation (8-4-4-4-12 hex digits)
    if (!grepl("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$", test_id)) {
        stop(
            arg_name, " must be a valid GUID (e.g. 'abcd1234-ab12-cd34-ef56-abcdef123456'). ",
            "You supplied: '", test_id, "'",
            call. = FALSE
        )
    }

    test_id
}

#' Build a ForceFrame test data frame
#'
#' Internal helper to convert a list of ForceFrame test records into a tidy \code{data.frame}.
#' For character columns, \code{NULL} values are replaced with an empty string (\code{""})
#' to ensure consistent downstream handling.
#'
#' Special handling is applied for the identifier column: some ForceFrame API endpoints
#' return this as \code{profileId}, while others return it as \code{athleteId}. This function
#' will populate the \code{profileId} column by taking the first non-empty value from
#' \code{profileId} or \code{athleteId}, in that order.
#'
#' @param records A list of ForceFrame test records from the API (parsed JSON).
#'
#' @return A \code{data.frame} where each row corresponds to a ForceFrame test record.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.build_forceframe_df <- function(records) {
    if (length(records) == 0) {
        return(data.frame())
    }

    data.frame(
        testId = .safe_extract(records, "testId"),
        profileId = .safe_extract_first(records, c("profileId", "athleteId")),
        testDateUtc = .safe_extract(records, "testDateUtc"),
        testTypeId = .safe_extract(records, "testTypeId"),
        testTypeName = .safe_extract(records, "testTypeName"),
        testPositionId = .safe_extract(records, "testPositionId"),
        testPositionName = .safe_extract(records, "testPositionName"),
        innerLeftAvgForce = .safe_extract(records, "innerLeftAvgForce"),
        innerLeftImpulse = .safe_extract(records, "innerLeftImpulse"),
        innerLeftMaxForce = .safe_extract(records, "innerLeftMaxForce"),
        innerLeftRepetitions = .safe_extract(records, "innerLeftRepetitions"),
        innerRightAvgForce = .safe_extract(records, "innerRightAvgForce"),
        innerRightImpulse = .safe_extract(records, "innerRightImpulse"),
        innerRightMaxForce = .safe_extract(records, "innerRightMaxForce"),
        innerRightRepetitions = .safe_extract(records, "innerRightRepetitions"),
        outerLeftAvgForce = .safe_extract(records, "outerLeftAvgForce"),
        outerLeftImpulse = .safe_extract(records, "outerLeftImpulse"),
        outerLeftMaxForce = .safe_extract(records, "outerLeftMaxForce"),
        outerLeftRepetitions = .safe_extract(records, "outerLeftRepetitions"),
        outerRightAvgForce = .safe_extract(records, "outerRightAvgForce"),
        outerRightImpulse = .safe_extract(records, "outerRightImpulse"),
        outerRightMaxForce = .safe_extract(records, "outerRightMaxForce"),
        outerRightRepetitions = .safe_extract(records, "outerRightRepetitions"),
        device = .safe_extract(records, "device"),
        notes = .safe_extract(records, "notes"),
        modifiedDateUtc = .safe_extract(records, "modifiedDateUtc"),
        stringsAsFactors = FALSE
    )
}

#' Build a NordBord test data frame
#'
#' Internal helper to convert a list of NordBord test records into a tidy \code{data.frame}.
#' For character columns, \code{NULL} values are replaced with an empty string (\code{""})
#' to ensure consistent downstream handling.
#'
#' Special handling is applied for the identifier column: some NordBord API endpoints
#' return this as \code{profileId}, while others return it as \code{athleteId}.
#' This function will populate the \code{athleteId} column by taking the first non-empty value
#' from \code{athleteId} or \code{profileId}, in that order.
#'
#' @param records A list of NordBord test records from the API.
#'
#' @return A \code{data.frame} with one row per NordBord test record.
#' Internal function (not designed to be used directly by end users)
#' @keywords internal
.build_nordbord_df <- function(records) {
    if (length(records) == 0) {
        return(data.frame())
    }

    data.frame(
        athleteId        = .safe_extract_first(records, c("athleteId", "profileId")),
        testId           = .safe_extract(records, "testId"),
        modifiedDateUtc  = .safe_extract_first(records, c("modifiedDateUtc", "modifiedUtc")),
        testDateUtc      = .safe_extract(records, "testDateUtc"),
        testTypeId       = .safe_extract(records, "testTypeId"),
        testTypeName     = .safe_extract(records, "testTypeName"),
        notes            = .safe_extract(records, "notes"),
        device           = .safe_extract(records, "device"),
        leftAvgForce     = .safe_extract(records, "leftAvgForce"),
        leftImpulse      = .safe_extract(records, "leftImpulse"),
        leftMaxForce     = .safe_extract(records, "leftMaxForce"),
        leftTorque       = .safe_extract(records, "leftTorque"),
        leftCalibration  = .safe_extract(records, "leftCalibration"),
        rightAvgForce    = .safe_extract(records, "rightAvgForce"),
        rightImpulse     = .safe_extract(records, "rightImpulse"),
        rightMaxForce    = .safe_extract(records, "rightMaxForce"),
        rightTorque      = .safe_extract(records, "rightTorque"),
        rightCalibration = .safe_extract(records, "rightCalibration"),
        leftRepetitions  = .safe_extract(records, "leftRepetitions"),
        rightRepetitions = .safe_extract(records, "rightRepetitions"),
        stringsAsFactors = FALSE
    )
}