This dataset includes federal civil cases from federal districts in the sixth, seventh, and eight circuits (i.e. midwestern district).   The cases where selected base on the following criteria:

1. Civil Rights Nature of Suit (NOS) Codes  
2. Filed from 2008 to 2019
3. Excludes Multi-District Litigation (MDL) cases

# Competition Data

## Training Dataset
Please use the following files to train the model for the prediction questions.

- **train_dockets.csv** - Metadata extracted from the case's docket and other judicial systems
- **train_termination_motions.csv** - Motions that if granted would close or move the case out of the federal district/office.  
- **train_other_motions.csv** - Motions that relate to the discovery process and or limits evidence.    Unlike the termination motions, this table includes the judge’s ruling on the motion.

## Test Dataset
Please use the following files to generate the summary judgment and settle predictions and probabilities

- **test_dockets.csv** - Metadata extracted from the case's docket and other judicial systems
- **test_termination_motions.csv** - Motions (if granted) that would end or transfer the case.   
- **test_other_motions.csv** - Motions related to the discovery process

## Sample Submission File (sample_submission.csv)

For each mudac_id provide two predictions with the following ids:

1.  mudac_id-SummaryJudgment - the probability the case is decided by a summary judgment
2. mudac_id-Settled - the probability the case was settled

## Supporting Files

- **district_fips_code.csv** - A mapping of the counties with in the US Court Districts.
- **districts.csv** - The district's population according to the 2010 US Census.

## Other Potential Datasources

- [US District Court Jurisdictions](https://www.kaggle.com/johnhudzina/us-district-court-jurisdictions)
- [FBI Uniform Crime Reporting (UCR) Program](https://www.fbi.gov/services/cjis/ucr)
- US Census

# Legal Dictionary (for this context)

**Plaintiff:** an individual who initiates a lawsuit against a defendant

**Defendant:** (civil) an individual, in litigation, who is sued by a plaintiff

**Jurisdiction:** may refer to (1) the location where a lawsuit is filed or the litigation occurs, or (2) the territory over which a court may hear lawsuits and make decisions that bind parties

**Compel:** to force another party to act or disclose information by requesting a court order

**Discovery:**  process of locating and learning new information in preparation for trial through documentation, question/answer sessions with participants and witnesses, and evidence requests between parties in a litigation

**Docket:** official record of the court noting the proceedings and filings in a court case

**Motion for Summary Judgment (MSJ):** asks the court to issue judgment without a full trial, on at least one claim

> **Standard to grant MSJ:** filer must show that they are entitled to the judgment because there are no facts in the existing filings that would allow the court to find against the filer of the MSJ

**Motion to Dismiss**: request for court to dismiss a case or claim

> Common reasons/standard for dismissal:  
-**Voluntary/non-suit dismissal:** plaintiff  
- **Settlement:** a settlement of the terms of the lawsuit are reached between the parties and the suit is dismissed under the settlement agreement  
- **Legal sufficiency:** the plaintiff has failed to meet standards that govern filing lawsuits regarding the location or process of filing a case  

**Motion to Consolidate:** a request for a court to combine two or more cases into a single case

> **Standard to grant:** if the cases involve the same parties and issues, the motion will generally be granted

**Motion to Remand:** request for a court to send back a case or claim to a lower court for further action and reconsideration of the issues

> **Standard:** when a higher court overrules and changes a lower court’s ruling, the instruction generally is sent back to the lower court to decide the subject matter of the case and its issues in light of the statements made by the higher court

**Motion to Transfer Venue:** request for the location of a case to be changed to a more proper location based on the power of a court over either the parties or the subject matter of the case

> **Standards/reasons to transfer:**
- If key events, important to the case, have occurred in the requested venue this may indicate a grant of transfer should occur - If the new venue would be more convenient to the parties and witnesses
- If justice would be better served

**Motion for Judgment on the Pleadings:** asks the court to issue judgment without a full trial, on at least one claim, based only on those documents already filed with the court (very similar to motion for summary judgment)

> **Standard to grant:** filer must show that they are entitled to the judgment because there are no facts in the existing filings that would allow the court to find against the filer

**Motion for Default Judgment:** request for the court to issue judgment based on the failure of the opposite party to take action (ex: to file an answering document, to appear in court, etc.)

**Motion for Discovery Sanctions:** request to the court to impose penalties for failure to disclose information which is in the possession of another party and related to the lawsuit

> **Standard to grant:** other party fails to follow order or court, attend discovery procedure, or make disclosure, without reasonable justification

**Sanction:** monetary or procedural penalty granted by the court resulting from failure to comply with a law, rule, or order of the court

> **Examples:** monetary penalty; dismissal of case is an extreme sanction rarely granted

**Motion for Injunction:** request for a court order that commands or prevents an action of another party or entity

> **Standard to grant:** requesting party must show that the legal process would not be sufficient, and that the order is necessary to prevent injury that could not be corrected

**Injunction:** the court order that would command or prevent action of a party

**Motion for Protective Order:** request for a court order that would 1) prevent disclosure of documents, or 2) restrict someone from harassing, annoying, or burdening another with their conduct

> **Standard to grant:** proof that party requesting the protective order has good cause and the order would protect them from annoyance, embarrassment, oppression, or excessive burden or expenses

**Motion for Temporary Restraining Order:** precursor to injunction, requesting the court order to preserve status of a claim until the injunction request can be heard by the court. Helps prevent time limits from running out while injunction hearing is pending.

> **Standard:** requesting party must show harm that would arise if the injunction does not issue, and the likelihood that they will prevail on the injunction request

**Motion in Limine:** request made before trial that evidence, which is not admissible in the case, cannot be referred to or offered as evidence during the trial portion of litigation

> **Standard:** the court determines whether a motion will be granted based solely on the persuasive legal arguments and authority offered by each party

**In Limine:** latin for “at the start” meaning the motion is filed prior to trial, during the pre-trial phase of litigation, and is used to exclude evidence from trial

**Motion to Certify Class:** request to the court to establish a “class,” made up of individuals, of a law suit

> **Standard to grant certification of class:** proof that 1) trying to name all plaintiffs would be impractical, 2) that all plaintiffs have common issues of law/fact, 3) the claims/defenses of the class representatives are typical of the class, and 4) that the representatives will fairly protect the interests of the entire class

**Class action:** court authorizes single person, or small group of persons, to represent interests of a larger group whose harm arises from the same issue or act

**Motion to Compel Arbitration:** request to the court to force another party to participate in the arbitration process  

> **Standard to grant:**
- Two parties must have an arbitration agreement to compel arbitration, generally
- One party may ask the court to require that the case first be arbitrated before a trial may occur – generally one party has refused or failed to participate at this point

**Arbitration:** method to resolve a dispute between two parties, outside of a court, where one or more neutral decisionmakers assists the parties in coming to an agreement. Similar to a settlement.

**Motion to Compel Discovery:** request that the court force an opposing party to provide documents or other materials related to the lawsuit which are in that party's possession          

> **Standard to grant:** The request is proper if: (1) a witness fails to answer (or evades) a proper question in deposition, (2) a corporation or other entity fails to designate a representative to speak for them, (3) a party fails to answer a written question; or (4) a party fails to produce documents or permit inspection of documents; and that the motion is submitted in good faith

**Granted:** request for a particular court order that is completely approved by the court

**Granted in Part:** order given by the court which approves some portions of a party's request for a court order and rejects others

**Denied:** court completely rejects a party's request for a court order    

**Denied as Moot (aka Moot):** the court rejects a party's request for a court order because the core issue of the request no longer exists

# Column Definitions

## train_dockets.csv

**mudac_id** Docket Identifier for MUDAC.

**circuit** The federal circuit the case was filed.

**district** The federal district the case was filed.

**office** The office with the federal district the case was filed.  The office locations are downloadable from the [free law project](https://free.law/idb-facts/).

**primarytitle** The case title.

**nos_code** The primary nature of suite as listed by the filing attorney.  While this code should represent the primary claims, the case may contain other claims unrelated to civil rights. This dataset is restricted to Civil Rights NOS codes only.

**nos_text** Textual description of the nature of suite code.

**statute** The federal statute cited as the cause when the complaint was filed.

**protected_class** A protected class explicitly reference as the cause of the suit by the plaintiff.

- Age
- Disability
- National Origin
- Race
- Religion
- Sex

**additional_nos** A secondary nature of suit.

**requested_damages_amt** The monetary amount sought by plaintiff (in thousands).  Money amounts less than $500 appear as 1, and amounts over $1,000,000 appear as 9999. Dollar figure is rounded to the nearest thousand.(eg.$1,234.56 would appear as a single digit (1).

**jury_demand** Indicates the party or parties demanding a jury trial.  

- B – Both plaintiff and defendant demand jury.
- D – Defendant demands jury.
- P – Plaintiff demands jury.
- N – Neither plaintiff nor defendant demands jury.  
- "-8" - missing

**jurisdiction**
The code which provides the basis for the U.S. district court jurisdiction in the case. This code is used in conjunction with appropriate nature of suit code.

- 1 - US government plaintiff
- 2 - US government defendant
- 3 - federal question
- 4 - diversity of citizenship
- 5 - local question

**diversity_residence** Involves diversity of citizenship for the plaintiff and defendant. First position is the citizenship of the plaintiff, second position is the citizenship of the defendant. See RESIDENC in the [Civil Code Book](https://www.fjc.gov/sites/default/files/idb/codebooks/Civil%20Codebook%201988%20Forward_0.pdf) for values.

**participants** Count of the participants listed on the PACER docket

**plaintiffs** Count of the plaintiffs listed on the PACER docket

**defendants** Count of the defendants listed on the PACER docket

**attorneys_listed** Count of the attorneys listed on the PACER docket

**judges_assigned** Count of the judges listed on the PACER docket

**pro_se** Is a party or parties representing themselves.

- 0 - no Pro Se plaintiffs or defendants
- 1 - Pro Se plaintiffs, but no Pro Se defendants
- 2 - Pro Se defendants, but no Pro Se plaintiffs
- 3 - both Pro Se plaintiffs & defendants
- -8 - missing

**informa_pauperis** Did the plaintiff apply for court fee wavier.

- FP = Informa Pauperis (IFP cases)  
- -8 = not IFP cases

**class_action** Involves an allegation by the plaintiff that the complaint meets the prerequisites of a "Class Action" as provided in Rule 23 - F.R.CV.P.

- True = Class Action
- False = Not Class Action

**arbitration_at_filing** This field is used only by the courts participating in the Formal Arbitration Program. It is not used for any other purpose.

Method of Participation:

- M - mandatory
- V - voluntary
- E - exempt
- Y - yes, but type unknown
- -8 – missing

**origin_at_filing** A single digit code describing the manner in which the case was filed in the district.  See ORIGIN in the [Civil Code Book](https://www.fjc.gov/sites/default/files/idb/codebooks/Civil%20Codebook%201988%20Forward_0.pdf) for values.

**filers_county** The county of residence of the first listed plaintiff ([FIPS County Code](https://www.census.gov/geographies/reference-files/2017/demo/popest/2017-fips.html)).

- If the US Government is the plaintiff, the county listed is that of the first listed defendant.
- If the location is within the U.S. but, outside the home state, the code is 88888.
- If the location is outside the U.S. the code is 99999.

**issue_joined** Boolean field indicating if the issue has been joined.

**pretrial_conf** Boolean field indicating if a pretrial conference has been held.  See [Rule 16](https://www.law.cornell.edu/rules/frcp/rule_16) for the purpose of a pretrial conference.

**total_entry_count** Count of the entries listed on the PACER docket.

**before_ij_entry_count** Count of the entries listed on the PACER docket before the FJC issue joined date.

**after_ij_entry_count** Count of the entries listed on the PACER docket on or after the FJC issue joined date.

**year_filed** The year the case was filed.

**days_opened** The number of days the case was opened.

**outcome** Simplified taxonomy of FJC’s disposition field.  While this column is not necessary for the prediction tasks, feel free to use this column error analysis and or an explanation.

**summary_judgment** Was the case closed by a summary judgment?  

**settled** Was the case closed by a settlement?

## train_termination_motions.csv

**mudac_id** Anonymized case identifier.

**motion_type** One of the following types:

- Motion to Dismiss
- Motion to Consolidate  
- Motion for Summary Judgment
- Motion to Remand
- Motion to Transfer Venue
- Motion for Judgment on the Pleadings
- Motion for Default Judgment

**filing_party** The role of the party that filed the motion. (Plaintiff, Defendant, Other, Unknown)

**filed_before_joined** Boolean that indicates if the motion was filed before the issue was joined.  Hint: Some motion can’t be filed before the issue is joined.

**proceeding_precentile** The number of related filings (brief, letters, replies, etc..) including the original motion.

## train_other_motions.csv

**mudac_id** Anonymized case identifier.

**motion_type** One of the following motion types:

- Motion for Discovery Sanctions
- Motion for Injunction
- Motion for Protective Order
- Motion for Temporary Restraining Order
- Motion in Limine
- Motion to Certify Class
- Motion to Compel Arbitration
- Motion to Compel Discovery

**filing_party** The role of the party that filed the motion. (Plaintiff, Defendant, Other, Unknown)

**filed_before_joined** Boolean that indicates if the motion was filed before the issue was joined.  Hint: Some motion can’t be filed before the issue is joined.

**decison** The judge’s decision about the motion.  This field may be null.  Values included:

- **Granted:** request for a particular court order that is completely approved by the court
- **Granted in Part:** order given by the court which approves some portions of a party's request for a court order and rejects others
- **Denied:** court completely rejects a party's request for a court order    
- **Denied as Moot (aka Moot):** the court rejects a party's request for a court order because the core issue of the request no longer exists
- **Other:** decision types excluded from this dataset

**decided_before_joined** Did the judge provide a decision before the issue was joined?

**proceeding_precentile** The number of related filings (brief, letters, replies, etc..) including the original motion.

# Other Links & Sources

- [Pulic Access to Court Electronic Records](https://www.pacer.gov/)
- [Court Listener - RECAP](https://www.courtlistener.com/recap/)
- [Federal Judicial Center - Inside Federal Courts](https://www.fjc.gov/education/inside-federal-courts)
- [Federal Judicial Center - Diversity on the Bench](https://www.fjc.gov/history/judges/diversity-bench)
- [Free Law Project](https://free.law/)
