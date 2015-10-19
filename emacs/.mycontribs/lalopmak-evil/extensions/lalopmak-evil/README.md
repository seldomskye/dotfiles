Lalopmak Evil
============

A variant of colemak-evil.el/colemak.vim that takes some of Shai's ideas even further.  This mapping aims to be even more geometric/movement-based, exploiting the directional intuition that makes us so good at playing games.  Ergonomicness is also a goal, while mnemonicness is not a primary goal (though I've attempted to bring some back).  Some more specific rationales can be found in [this file](https://github.com/lalopmak/lalopmak-evil/blob/master/rationales.md).

Draft diagrams can be found near the top of [this file](https://raw.github.com/lalopmak/lalopmak-evil/master/lalopmak-evil-libraries.el).



Setup
-----

I use [melpa](http://melpa.milkbox.net/) for many of these packages.  Your mileage may vary.

Depending on the package/your personal needs, you may have to initialize it in your init.el.  Most packages have to at least be `(require '...)` in order to work.

1. Install: [Evil](http://gitorious.org/evil/pages/Home#Install), ace-jump-mode.
2. Download lalopmak-evil and put it somewhere in your load path.
3. Add `(require 'lalopmak-evil)` to your Emacs init file.

Recommended: linum-relative, centered-cursor-mode, yasnippet, speck, evil-surround


Experimental: If you wish to use this with QWERTY, add `(defvar lalopmak-layout-map 'colemak-to-qwerty)` to your init.el file, prior to the load.  If you want to use it with another layout, you currently have to define your own colemak-to-layout map.

Tips
----
Type :hints or :ars to bring up the hint screen.  Type :mnemonic to bring up mnemonic hintscreen.  Not all content is the same in both.

Escape takes you into normal mode, an unfortunate historical accident.
I recommend defining an easy AltGr mapping (I use AltGr+t) since it will
be used a lot.  Alternatively, see patbl's [advice on key chords](https://github.com/patbl/colemak-evil/blob/master/README.md).

Mnemonic Mappings
----

I am also experimenting with mnemonics-preserving minimal changes that integrate some of these features.  To try these out, `(require 'lalopmak-evil-mnemonic)` instead.

Disclaimer
----

THIS SOFTWARE AND ANY ASSOCIATED DOCUMENTATION ARE PROVIDED BY THE COPYRIGHT 
HOLDERS AND CONTRIBUTORS "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, 
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER 
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE OR ANY ASSOCIATED 
DOCUMENTATION, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Colemak Evil
============

If you would, instead, like colemak-evil, please see the parent repository: https://github.com/patbl/colemak-evil

Colemak Evil is a set of remappings that implements some of
Shai Coleman's awesome Vim remappings in Emacs
([more information](http://forum.colemak.com/viewtopic.php?id=50)).

It's usable, but I'm an expert in neither Vim nor Emacs so you'll
likely encounter some funky behavior. If you have any improvements,
I'd be glad to integrate them.

Here are a few of the main differences from Shai's mappings:

* The only Vim mapping that works in insert mode is Esc (this avoids
  conflicts with Emacs's shortucts).
* Folding and several other features aren't implemented.
* Tab in insert mode doesn't take you into normal mode. 