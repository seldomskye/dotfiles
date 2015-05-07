License
============

This document is licensed under the CC0 1.0 Public Domain Declaration, as
released by Creative Commons <http://creativecommons.org/publicdomain/zero/1.0/>.

THIS DOCUMENT IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS",
WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO
THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
DOCUMENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Introduction
============

NB: This is NOT an expert opinion - I am an amateur when it comes to these subjects.  I am mainly writing this down so that I don't forget later.

This document gives some explanations as to why I chose current positions over, say, default colemak.vim/colemak-evil.  Though I adopted the classic unei movement keys that distinguish this class of mappings, colemak.vim is still overly reliant on mnenomics, leading to questionable key placements like r (replace) and o (open).  I attempt to improve upon these at the further cost of mnemonics, aiming for ergonomicness and geometric suggestiveness (though the latter has been reduced since find-char was made redundant).

Overview
============

Most obviously, many formerly shifted keys are instead modified with C-.  This is because, on my keyboard (as well as probably most emacs users') C- is bound to caps lock, an easier key to reach.

Though I considered leaving b/B (buffer/file) as a ; command, switching buffers is ultimately too common.

Navigation keys
----

NI were changed to forward/backward sentence, UE forward/backward paragraph.  The previous 5x bindings were kinda redundant.  There might be better choices for this.

Top Left (qwfp/arst)
----

Change was moved to t, due to its very common usage, especially comboed with ace-jump (wf).  It is also commonly comboed with rs for inner/outer text objects. The close proximity to delete (d) is coincidential, but enhances suggestiveness.

:/;
----
; (evil-ex) is probably the most common command.  Moving it from default placement is slightly problematic in that it may not be moved in other apps ("legacy issues").  But more importantly, it does not seem to gain from a home-row spot.  All usages of ; end with <RET>, which current positioning of ; (hit by ring finger) helps move ring finger toward.  ; commands also tend to be short so that this gain is not generally neutralized.  For example, ;c<RET> is much more efficient than oc<RET>, because after hitting ; the pinkie is already primed to hit <RET>.

ozxp, and possibly q
----

undo moved to p/C-p.  z/Z is okay and has nice shift properties.  But it might not be very good considering the frequency of usage.

The stats for undo are sort of inflated because I spam it so much to get rid of a bunch of changes, but it's not really used in tandem with other commands, so an out-of-the-way key is okay.  What is required is that the key be used by a strong finger (one that, once the hand positioning has been shifted, is easy to press repeatedly.  z/Z requires too much folding of the ring finger to make this happen, and v did not work well in practice (though its mirror, k, is spammed a lot, so this might just be due to practice).  The fact that it is awkward to move away from p to other letters reduces its opportunity cost.

open above/below are not used nearly often enough to deserve home row spots.  Currently, the spots open are qzx, of which x is out of the question.  z/Z is not a bad spot, especially for such a "nonflowing command", and moves finger toward shift which is somewhat common when opening new line, but it is an open question whether it is preferable over q/C-q, and also whether the relative frequencies justify best placement.

open's old "o" position was replaced by ace-jump-line. Its an improvement over its original p position, which is awkward for a "flowing command", since it requires reaching back the index-finger.  It's also not paired with <RET>, eliminating a possibility of repeated finger, though the repeated possibility does come up in the ace-jumping itself.

substitute currently at x.  Questionable placement, though one advantage is that x works "as expected" as a selection "cut".  Alternatives include q and o.  Will depend on usage frequencies.

Possible Improvements
============

Swapping paste/undo to p/v
----
Call paste/undo at v/p "default", p/v "swapped".

Undo is generally used much more often than paste (though usually in succession rather than randomly, which might affect the analysis) , so it should get "the better key".  It's questionable, however, just which key that is.

p is harder to reach than v, on a random basis [swapped].  The p requires awkward repositioning.  Once repositoned, however, repeat-pressing does not seem to be an issue [neutral].  In practice, I had trouble repeat-pressing v, whereas not p [default].  This contrasts severely with repeating k, which suggests this might be a matter of practice.

v is farther than p from all modifiers [swapped].  However, when pressing v the pinkie seems to curl to more easily press the shift, suggesting that V is next-most easlily pressed, then C-v.  By contrast, when moving up to press p, the pinkie is positioned to press C-p, and P seems harder to press at.  This suggests that v is the better key for using with both modifiers [default].

Redoing the movement keys
====

h (scroll down, ~8%) and j (scroll up; ~4%) are very common, much moreso than forward/backward char (~2%/2%) and especially forward/backward word (~1%/1%).  A possible improvement would be to change them to reflect the "browsing" setup:

scroll up/down - ni

arrow keys: luye

forward/backward word: jh

That way, one would not have to leave the home-row to enact the most common scrolling.

Against this, one should note that jh are, similarly to undo, "inflated", not used very often in tandem with other keys, but rather "spammed" by a strong finger once the hand position is shifted.  In this new setup, the ring finger would be responsible for an additional sustained 8% key (even if one hypothesizes switching directions, 4%).  Adding to the 7% use of ; (evil-ex), we get:

Finger usage stats (index ring):
Original: 15% 10%
Proposed: 8% 17%
Switched: 12% 13%

By comparison, the middle finger gets 4%.

The "switched" setup leads to the most balanced load, but is ungeometric and puts an arguably unjustified load onto the ring finger.  The original minimizes that load.

Swap ex to h
----

Another, possibly better setup would be to start with the previous Proposed, and swap ; with h.  Thereby, forward/backward word would be at j;, while ex at h. This arguably puts the seldom-used words at their best points, while moving the common evil-ex to a stronger finger.

Swapped: 14% 11%
Swapped-switched: 18% 7%

In addition to the slightly increased use of the ring finger, a possible disadvantage is as detailed in the :/; section: moving ex to home-row (and an inner-home row, to boot) makes certain common ex commands less efficient, since the movement, in this case, moves the pinkie away from the <RET> key.



Switch copy and paste?
====
Copy is a motion (though probably one of the lesser-used ones), hence more likely to be paired with others keys.  v provides a better position for this than c.

Ultimately, the defeater of this position is that C-c seems to be used for other keybindings, hence not leaving room for paste-pop.  Thankfully, copy's use frequency (1.3% as of this writing vs about 1.7% for the pastes; many of these also probably require visual mode, negating this issue) is low enough that this is probably not a big deal.  If this ever changes, I may have to consider more drastic action.
