import Types "../types/property-listings";
import Lib "../lib/property-listings";
import List "mo:core/List";

mixin (
  submissions : List.List<Types.PropertySubmission>,
  submissionCounter : { var value : Nat },
) {
  public func submitProperty(submission : Types.PropertySubmission) : async Text {
    await* Lib.submitProperty(submissions, submissionCounter, submission);
  };

  public query func getSubmissions() : async [Types.PropertySubmission] {
    Lib.getSubmissions(submissions);
  };
};
