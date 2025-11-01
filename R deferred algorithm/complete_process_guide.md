# Complete Club Assignment Process Guide
## For Parents, Students, and Teachers

---

## Overview

Our school uses a fair, computerized system to assign students to clubs. This ensures every student gets clubs they're interested in while respecting each club's capacity limits. The process takes into account **student preferences**, **club capacities**, and **hour requirements**.

---

## BEFORE THE ALGORITHM RUNS

### What Students Must Do

**Step 1: Receive the Club List**
- You'll receive a list of all 59 available clubs
- Each club shows:
  - **Club name** (e.g., "Barcelona Photography Club")
  - **Hours per week** (either 2 or 4 hours)
  - **Brief description** of activities
  - **Meeting times**

**Step 2: Rank ALL Clubs**
- You must rank **all 59 clubs** in order of preference
- **1 = your favorite club** (most interested)
- **59 = your least favorite club** (least interested)
- **Important:** Be honest! Rank clubs you truly want to join higher
- **Tip:** Research clubs carefully - ask current members, read descriptions

**Step 3: Submit Your Rankings**
- Submit your complete ranking list by the deadline
- **No changes** allowed after submission
- Late submissions may not be processed

**Requirement:** Each student must accumulate **exactly 10 hours per week** across all their assigned clubs.

---

### What Teachers Must Do

**Step 1: Prepare Club Information**
- Create or update the club information sheet with:
  - Club name and description
  - Hours per week the club meets (2 or 4 hours)
  - **Desired number of students** (between 10-22 students)
  - Meeting schedule and location
  
**Step 2: Set Club Capacities**
- Decide how many students each club can accommodate
- Consider:
  - **Physical space** limitations (room size, equipment)
  - **Teaching capacity** (can you effectively teach 22 students?)
  - **Minimum viability**: All clubs need at least 10 students to run
  - **Maximum allowed**: No club can exceed 22 students
  
**Example:**
- Art Studio with limited easels → set capacity to 12 students
- Soccer Club with a large field → set capacity to 22 students
- Debate Club with intensive coaching → set capacity to 15 students

**Step 3: Provide Information to Administrators**
- Submit all club details in the required format (CSV file)
- Include: club_id, club_name, hours_per_week, club_students

---

## HOW THE ALGORITHM WORKS

### Phase 1: Initial Deferred Acceptance (Rounds 1-50+)

Think of this like a sophisticated "proposal and acceptance" dance:

**Round 1:**
1. **Every student proposes** to their #1 ranked club (their favorite)
2. **Each club receives** all proposals from students who ranked it #1
3. **Each club evaluates** all proposals by looking at how highly students ranked them
   - A student who ranked the club #1 gets priority over a student who ranked it #3
4. **Each club tentatively accepts** its top students up to its desired capacity
   - Example: If "Barcelona Photography" has capacity for 15 students but receives 30 proposals, it accepts the 15 students who ranked it highest
5. **Rejected students** move to their next choice

**Round 2 and Beyond:**
- Students who were rejected propose to their **next-best club** on their list
- Students who were accepted but **still need more hours** (haven't reached 10 yet) also propose to their next choice
- Students who have **exactly 10 hours stop** proposing - they're done!

**How Clubs Decide:**
- Each club looks at **ALL current proposals** (new ones + students tentatively accepted from previous rounds)
- Clubs can **change their minds**! A student accepted in Round 5 might be displaced in Round 10 by someone who ranked the club higher
- Clubs always keep their **top-ranked students** (students who gave them the highest rankings)
- Rejected students are free to propose elsewhere

**Smart Features:**
- Students only propose to clubs that **won't push them over 10 hours**
  - If you have 8 hours, you only propose to 2-hour clubs (not 4-hour clubs)
  - If you have 6 hours, you can propose to both 2-hour and 4-hour clubs
- The algorithm continues until all (or nearly all) students reach exactly 10 hours

**Why This Is Fair:**
- You always get the **best possible combination** of clubs given everyone's preferences
- Popular clubs naturally fill up with students who really want to be there
- No favoritism - the computer follows the same rules for everyone
- Mathematically proven to be optimal (Nobel Prize-winning algorithm!)

---

### Phase 2: Progressive Elimination (If Needed)

After the initial matching, some clubs might have very few students. Here's what happens:

**Why Some Clubs Might Be Small:**
- Not enough students ranked them highly
- Students who wanted them couldn't fit them into their 10-hour schedule
- Similar clubs split the available students

**Progressive Elimination Process:**

The algorithm gradually closes clubs that can't reach minimum viability:

1. **Threshold 2**: Clubs with only 1 student are closed
   - These students lose those hours and must re-register
   
2. **Threshold 3**: Clubs with 2 students are closed
   - Affected students lose those hours
   
3. **Continues up to Threshold 9**: Clubs with 8 students are closed

**What Happens to Affected Students:**
- When your club closes, you lose those hours
- You immediately re-enter the assignment process
- You propose to your **next-best available clubs** from your original ranking
- The Deferred Acceptance algorithm runs again for you (mini-rounds)
- You get reassigned based on your preferences

**Why This Helps:**
- Prevents very small classes that aren't educationally viable
- Consolidates students into clubs with sufficient enrollment
- Ensures teacher time is used efficiently
- Students still get their next-best preferences

---

### Phase 3: Priority Filling

After progressive elimination, some clubs might have 8-9 students (close to the minimum):

**Priority Filling Process:**
1. Identify clubs with 8-9 students (need 1-2 more to reach minimum 10)
2. Find students who:
   - Already have 10 hours (they're complete)
   - Ranked this struggling club reasonably high (within their top 30 choices)
   - Are not already in this club
3. Add these students to help the club reach 10 students
4. These students now have 12 hours (above the 10 requirement)

**Important Note:**
- Only a small number of students will be asked to take on extra hours
- You'll only be added to clubs you ranked favorably
- This helps ensure no clubs are canceled at the last minute
- Teachers can identify these students if they prefer to opt-out

---

## AFTER THE ALGORITHM RUNS

### What Students Receive

You'll get a notification showing:
- **List of your assigned clubs** with club names
- **Total hours per week** (should be 10 or very close)
- **Meeting times** for each club
- **Instructions** for the first meeting

**Example Assignment:**
```
Student: Maria Papadopoulos
Club Assignments:
1. Barcelona Photography Club (4 hours/week)
2. Athens Debate Team (2 hours/week)  
3. Lyon Cooking Club (2 hours/week)
4. Vienna Music Ensemble (2 hours/week)

Total: 10 hours/week
```

### What Teachers Receive

Teachers get a roster for their club showing:
- **Final enrollment count**
- **Student names and contact information**
- **Meeting schedule**
- **Classroom assignment**

---

## FREQUENTLY ASKED QUESTIONS

### For Students and Parents

**Q: Why didn't my child get their first choice club?**

A: Your child's first choice was very popular. Many students ranked it highly, and it filled up with students who ranked it #1 or #2. Your child's current clubs are the best available options given everyone's preferences and the capacity constraints.

**Q: My child has 12 hours instead of 10. Why?**

A: Your child was selected to help a club reach its minimum enrollment. They ranked this club favorably in their preferences, and only a few students are asked to do this. If this creates a hardship, please contact the administration.

**Q: Can we request a change after assignments?**

A: The algorithm has already found the best possible assignment for everyone. Manual changes could create a cascade of problems. However, if there are **special circumstances** (medical needs, family emergency, transportation issues), please contact us immediately.

**Q: What if a club my child was assigned to gets canceled?**

A: If a club is canceled after final assignments, affected students will be reassigned to their next-best available clubs with spaces.

**Q: My child ranked a club #5 but didn't get it, yet got a club they ranked #30. Why?**

A: The club ranked #5 probably filled with students who ranked it even higher (#1-4). Clubs ranked #6-29 might have been 4-hour clubs that would have pushed your child over 10 hours, or they filled with other students. The algorithm ensures you get the best **combination** that adds up to 10 hours.

---

### For Teachers

**Q: My club has only 11 students but I requested 20. Can I get more?**

A: The algorithm gave you the students who ranked your club highest among available options. If 11 students strongly wanted your club, that's the optimal enrollment. You can make your club more attractive next year through better marketing and descriptions.

**Q: Can I manually add or remove students?**

A: No. The algorithm creates a stable matching where no student can improve their situation without making another student worse off. Manual changes would violate this fairness principle.

**Q: A student in my club seems unhappy. What should I do?**

A: Talk with them! They might have misconceptions about the club. If there's a genuine problem, they can speak with administration. Remember: they ranked your club somewhere on their list, so there was initial interest.

**Q: What if I can't accommodate the number of students assigned?**

A: You should have set your club's capacity accurately in the planning phase. If there's a genuine emergency (room reassignment, equipment failure), contact administration immediately before assignments are finalized.

**Q: My club has exactly 10 students. Is it at risk of cancellation?**

A: No. 10 students is the minimum for viability. Your club is secure. Clubs are only at risk during the elimination phase if they're below 10 students.

---

## TIMELINE

**Week 1:** 
- Teachers submit club information and capacities
- Students receive club descriptions

**Week 2:** 
- Students research clubs and prepare rankings
- Questions answered by teachers and administrators

**Week 3:** 
- **Deadline:** Students submit their complete rankings
- No changes allowed after deadline

**Week 4:**
- Algorithm runs (usually takes a few hours)
- Results reviewed by administration
- Adjustments made if needed

**Week 5:**
- Students receive assignments
- Teachers receive rosters
- First club meetings begin

---

## THE MATHEMATICS BEHIND IT

This system uses the **Deferred Acceptance Algorithm**, invented by economists Alvin Roth and Lloyd Shapley, who won the **2012 Nobel Prize in Economics** for this work.

**What makes it special:**
- **Student-optimal**: Students get the best possible outcome they can achieve
- **Stable**: No student and club would rather be matched with each other than their current assignments
- **Strategy-proof**: Being honest in your rankings gives you the best result (no benefit to gaming the system)
- **Fair**: Everyone follows the same rules with no favoritism

This same algorithm is used worldwide for:
- Medical residency matching (doctors to hospitals)
- School admissions in major cities
- Kidney donor matching
- College housing assignments

---

## IMPORTANT REMINDERS

✅ **Rank honestly** - Put your real favorites first
✅ **Rank all 59 clubs** - The algorithm needs your complete preferences
✅ **Meet the deadline** - Late submissions cannot be accommodated
✅ **Trust the process** - The algorithm is mathematically proven to be fair
✅ **Read club descriptions carefully** - Make informed choices
✅ **Ask questions early** - Don't wait until the deadline

❌ **Don't** try to strategize or game the system - honesty works best
❌ **Don't** submit incomplete rankings
❌ **Don't** expect changes after assignments are made
❌ **Don't** assume you'll get your first choice - rank many clubs you'd enjoy

---

## CONTACT INFORMATION

**For questions about:**
- **The ranking process**: Student Affairs Office
- **Club descriptions**: Individual club advisors
- **Technical issues**: IT Support
- **Special accommodations**: Dean of Students
- **The algorithm itself**: Administration Office

**We're here to help ensure every student has a great club experience!**

---

*This document explains the club assignment process used at [School Name]. The algorithm ensures fair, preference-based assignments while maintaining viable club sizes and meeting each student's 10-hour requirement.*
