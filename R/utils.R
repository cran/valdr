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

#' Build long-format mapping of ForceDecks test attributes
#'
#' Given a list of test records (from parsed JSON), this returns one row per
#' test-attribute combination, with a stable set of columns.
#'
#' Only attributes with at least one non-blank field are included. Tests with
#' no attributes (or only blank attributes) do not appear in the result.
#'
#' @param records list of test records (each may contain $attributes)
#' @return data.frame with columns:
#'   - testId
#'   - attributeTypeId
#'   - attributeTypeName
#'   - attributeValueId
#'   - attributeValueName
#' @keywords internal
.build_test_attributes_long <- function(records) {
    # Empty input = empty but correctly typed data frame
    if (length(records) == 0L) {
        return(data.frame(
            testId             = character(0),
            attributeTypeId    = character(0),
            attributeTypeName  = character(0),
            attributeValueId   = character(0),
            attributeValueName = character(0),
            stringsAsFactors   = FALSE
        ))
    }

    rows <- list()

    for (rec in records) {
        test_id <- rec$testId
        if (is.null(test_id)) test_id <- ""

        attrs <- rec$attributes

        # No attributes = nothing to add for this test
        if (is.null(attrs) || length(attrs) == 0L) {
            next
        }

        for (a in attrs) {
            if (is.null(a)) next

            type_id <- if (is.null(a$attributeTypeId)) "" else as.character(a$attributeTypeId)
            type_name <- if (is.null(a$attributeTypeName)) "" else as.character(a$attributeTypeName)
            val_id <- if (is.null(a$attributeValueId)) "" else as.character(a$attributeValueId)
            val_name <- if (is.null(a$attributeValueName)) "" else as.character(a$attributeValueName)

            # Skip rows where all attribute fields are blank
            all_blank <- (
                (type_id == "" || is.na(type_id)) &&
                    (type_name == "" || is.na(type_name)) &&
                    (val_id == "" || is.na(val_id)) &&
                    (val_name == "" || is.na(val_name))
            )
            if (all_blank) {
                next
            }

            rows[[length(rows) + 1L]] <- list(
                testId             = test_id,
                attributeTypeId    = type_id,
                attributeTypeName  = type_name,
                attributeValueId   = val_id,
                attributeValueName = val_name
            )
        }
    }

    if (length(rows) == 0L) {
        return(data.frame(
            testId             = character(0),
            attributeTypeId    = character(0),
            attributeTypeName  = character(0),
            attributeValueId   = character(0),
            attributeValueName = character(0),
            stringsAsFactors   = FALSE
        ))
    }

    # Use .safe_extract for consistency
    data.frame(
        testId             = .safe_extract(rows, "testId"),
        attributeTypeId    = .safe_extract(rows, "attributeTypeId"),
        attributeTypeName  = .safe_extract(rows, "attributeTypeName"),
        attributeValueId   = .safe_extract(rows, "attributeValueId"),
        attributeValueName = .safe_extract(rows, "attributeValueName"),
        stringsAsFactors   = FALSE
    )
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

#' Default export directory (Downloads/VALD_Exports)
#'
#' Internal helper to resolve the user's Downloads folder in a cross-platform
#' friendly way, and append a VALD_Exports subfolder. Falls back to a temp
#' directory if needed, with informative messages.
#'
#' @keywords internal
.vald_default_export_dir <- function() {
    # Try to resolve home directory
    home <- Sys.getenv("USERPROFILE", unset = NA)
    if (is.na(home) || !nzchar(home)) {
        home <- Sys.getenv("HOME", unset = NA)
    }

    if (is.na(home) || !nzchar(home)) {
        message(
            "Could not determine a home directory from USERPROFILE/HOME. ",
            "Falling back to a temporary directory for exports."
        )
        base_dir <- tempdir()
    } else {
        downloads <- file.path(home, "Downloads")

        if (!dir.exists(downloads)) {
            message(
                "'Downloads' folder not found at: ", downloads, ". ",
                "Falling back to a temporary directory for exports."
            )
            base_dir <- tempdir()
        } else {
            base_dir <- downloads
        }
    }

    export_dir <- file.path(base_dir, "VALD_Exports")

    # Ensure the export directory exists (or can be created)
    if (!dir.exists(export_dir)) {
        ok <- tryCatch(
            dir.create(export_dir, recursive = TRUE),
            warning = function(w) {
                message(
                    "Warning while creating export directory at: ", export_dir,
                    " - ", conditionMessage(w)
                )
                FALSE
            },
            error = function(e) {
                message(
                    "Error while creating export directory at: ", export_dir,
                    " - ", conditionMessage(e)
                )
                FALSE
            }
        )

        if (!ok) {
            # Last-resort fallback: tempdir()/VALD_Exports
            fallback_base <- tempdir()
            fallback_dir <- file.path(fallback_base, "VALD_Exports")

            message(
                "Falling back to temporary export directory at: ",
                fallback_dir
            )

            if (!dir.exists(fallback_dir)) {
                ok_fb <- tryCatch(
                    dir.create(fallback_dir, recursive = TRUE),
                    error = function(e) {
                        stop(
                            "Failed to create fallback export directory at: ",
                            fallback_dir, " - ", conditionMessage(e),
                            ". If this error persists, please specify your desired ",
                            "export folder path explicitly via the folder/path argument ",
                            "in the export function you are calling. This error messages are ",
                            "only coming from a function used to determine a default location when none is provided.",
                            call. = FALSE
                        )
                    }
                )
            }

            export_dir <- fallback_dir
        }
    }

    export_dir
}
