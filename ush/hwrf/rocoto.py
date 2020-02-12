"""!This module contains utilities for plugging HWRF into the Rocoto
workflow manager."""

##@var __all__
# List of symbols exported by "from hwrf.rocoto import *"
__all__=['cycles_as_entity']

import StringIO, random
import hwrf.numerics

from hwrf.numerics import to_datetime, to_timedelta

##@var epsilon
# An epsilon value for time equality comparisons
epsilon=to_timedelta(5)

##@var six_hours
# A datetime.timedelta that represents +6 hours
six_hours=to_timedelta(6*3600)

##@var sanity_quotes
# A list of quotes to print (one at random) when a sanity check fails.
# The purpose of this is to assure the user that he/she/it is not alone
# in lacking sanity; and that sanity, perhaps, is not something that
# one should strive for anyway.
# @hideinitializer
sanity_quotes=list([ 
        '''"I am thankful the most important key in history was invented. It's
not the key to your house, your car, your boat, your safety deposit
box, your bike lock or your private community. It's the key to order,
sanity, and peace of mind. The key is 'Delete.'"  -- Elayne Boosler''',
        '''"For me, insanity is super sanity. The normal is psychotic. Normal
means lack of imagination, lack of creativity."  -- Jean Dubuffet''',
        '''"Too much sanity may be madness and the maddest of all, to see life as
it is and not as it should be."  -- Miguel de Cervantes''',
        '''"One of the definitions of sanity is the ability to tell real from
unreal. Soon we'll need a new definition."  -- Alvin Toffler''',
        '''"You have to keep your sanity as well as know how to distance yourself
from it while still holding onto the reins tightly. That is a very
difficult thing to do, but I'm learning."  -- Diahann Carroll''',
        '''"The statistics on sanity are that one out of every four Americans is
suffering from some form of mental illness. Think of your three best
friends. If they're okay, then it's you."  -- Rita Mae Brown''',
        '''"[HWRF is] a camel designed by a comedian."
 -- unattributed''',
        '''"Well, sanity, I suppose, is getting people to see the world your
way." -- Paul Merton''',
        '''"Our sense of worth, of well-being, even our sanity depends upon our
remembering. But, alas, our sense of worth, our well-being, our sanity
also depend upon our forgetting." -- Joyce Appleby''',
        '''"I had a quick grasp of the secret to sanity, it had become the
ability to hold the maximum of impossible combinations in one's mind."
-- Norman Mailer''',
        '''"Sanity calms, but madness is more interesting." -- John Russell''',
        '''"Sanity is surely not about normality in the statistical sense: it is
about an eternal and natural idea of the healthy personality - which
indeed may be a rare achievement." -- Michael Leunig''',
        '''"Sanity is madness put to good use." -- George Santayana''',
        '''"In a mad world, only the mad are sane." -- Akiro Kurosawa''',
        '''"We are all born mad. Some remain so." -- Samuel Beckett''',
        '''"Insanity is doing the same thing in the same way and
expecting a different outcome." -- Chinese Proverb''',
        '''"I feel like a fugitive from the law of averages."
 -- William H. Mauldin''',
        '''"You should listen to your heart, and not the voices in your head."
 -- Matt Groening''',
        '''"No excellent soul is exempt from a mixture of madness." -- Aristotle''',
        '''"Everything great in the world is done by neurotics; they alone
founded our religions and created our masterpieces." -- Marcel Proust''',
        '''"Today I felt pass over me a breath of wind from the wings of
madness." -- Charles Baudelaire''',
        '''"When we remember we are all mad, the mysteries disappear and life
stands explained." -- Mark Twain''',
        '''"Without bigots, eccentrics, cranks and heretics the world would not
progress." -- Frank Gelett Burgess''',
        '''"Correct me if I'm wrong, but hasn't the fine line between sanity and
madness gotten finer?" -- George Price''',
        '''"Ordinarily he was insane, but he had lucid moments when he was merely
stupid." -- Heinrich Heine''',
        '''"Oh, that way madness lies; let me shun that." -- William Shakespeare''',
        '''"You're only given a little spark of madness. You mustn't lose it."
 -- Robin Williams''',
        '''"There's a fine line between genius and insanity. I have erased this
line." -- Oscar Levant''',
        '''"Sanity is not statistical." -- George Orwell''',
        '''"Part of being sane, is being a little bit crazy." -- Janet Long''',
        '''"I don't really trust a sane person." -- Lyle Alzado''',
        '''"Truly great madness cannot be achieved without significant intelligence."
 -- Henrik Tikkanen''',
        '''"Might we not say to the confused voices which sometimes arise from
the depths of our being, 'ladies, be so kind as to speak only four at
a time?'" -- Madame Swetchine''',
        '''"Insanity is often the logic of an accurate mind overtasked."
  -- Oliver W. Holmes''',
        '''"Flibbidy Jibbit." -- HWRF Sanity Checker''',
        '''"I became insane, with long intervals of horrible sanity."
 -- Edgar Allan Poe''',
        '''"Sanity and happiness are an impossible combination."
 -- Mark Twain''',
        '''"One person's craziness is another person's reality."
 -- Tim Burton''',
        '''"Don't worry. You're just as sane as I am."
 -- J.K. Rowling, "Harry Potter and the Order of the Phoenix"''',
        '''"Here lies a nuisance dedicated to sanity." -- David Low''',
        '''"Sanity is only that which is within the frame of reference of 
conventional thought." -- Erich Fromm''',
        '''"If you think anyone is sane you just don't know enough about 
them." -- Christopher Moore, "Practical Demonkeeping"''',
        '''"I mean, maybe I am crazy. I mean, maybe. But if this is all 
there is, then I don't want to be sane."
 -- Neil Gaiman, "Neverwhere"''',
        '''"Show me a sane man and I will cure him for you." -- C.G. Jung''',
        '''"When you're the only sane person, you look like the only insane 
person." -- Criss Jami, "Diotima, Battery, Electric Personality"''',
        '''"When the whole world is crazy, it doesn't pay to be sane."
 -- Terry Goodkind, "The Pillars of Creation"''',
        '''"Sanity is a cozy lie." -- Susan Sontag''',
        '''"One must be sane to think clearly, but one can think deeply and 
be quite insane." -- Nikola Tesla''',
        '''"When dealing with the insane, the best method is to pretend to 
be sane." -- Hermann Hesse''',
        '''"Perfect sanity is a myth propagated by straitjacket salesmen."
 -- Rebecca McKinsey''',
        '''"As long as you doubt your sanity, you can't be insane."
 -- Miles Keaton Andrew''',
        '''"Sometimes, to regain sanity, one had to acknowledge and embrace
the madness." -- Morgan Rhodes, "Rebel Spring"''',
        '''"Sanity is transmuting all the insane parts of your brain into 
a creative outlet" -- Jaeda DeWalt''',
        '''"Reality is a hallucination shared by most sane men."
 -- Mokokoma Mokhonoana''',
        '''"Sanity? Sorry, but I don't remember having such a useless 
thing in the first place" -- Tite Kubo''' ])
"""A set of famous quotes about sanity.  These are printed by the
sanity_check_failed function after a failure of a sanity check.  The
purpose is to give the user a measure of emotional support after
failure to configure an inevitably complex system."""

def entity_quote(string):
    """!Returns a copy of the string with & " < % and > replaced with
    their respective XML entities &#38; &#34; &#60; &#37; and &#62;
    @param string the string to parse.
    @returns a new string with proper replacements"""
    return string.replace('&','&#38;') \
                 .replace('"','&#34;') \
                 .replace('<','&#60;') \
                 .replace('%','&#37;') \
                 .replace('>','&#62;')

def sanity_check_failed(logger,ex):
    """!Logs information about a failure of a sanity check routine.
    The failure is in exception ex, and the "logger" argument must be
    a logging.Logger.
    @param ex the failure information
    @param logger a logging.Logger for log messages"""
    quote=sanity_quotes[random.randint(0,len(sanity_quotes)-1)]
    logger.critical('Sanity check failed.',exc_info=True)
    logger.info('\n'+quote+'\n'
        'HWRF SANITY CHECK FAILED.  Cannot run this configuration.\n'
        'Check paths and conf files.  See earlier messages for details.')

def cycles_as_entity(cycleset):
    """!Returns a set of Rocoto XML <cycledef> tags to add to an XML
    file.  The tags will define the list of cycles specified in the
    cycleset argument.  That argument must be a set of datetime
    objects.
    @param cycleset an iterable of cycles to convert.  These can be
      anything accepted by hwrf.numerics.to_datetime()"""
    cycles=list(cycleset)
    cycles = [ to_datetime(x) for x in cycles ]
    cycles.sort()
    ctream=StringIO.StringIO()
    first=cycles[0]
    last=cycles[0]
    sent=cycles[0]-six_hours
    for cycle in cycles:
        if to_datetime(cycle) > to_datetime(last)+six_hours+epsilon:
            # Found a break in the cycles
            writeme='<cycledef>%s00 %s00 06:00:00</cycledef> ' \
                %(first.strftime('%Y%m%d%H'),
                  last.strftime('%Y%m%d%H'))
            ctream.write(writeme)
            sent=last
            first=cycle
            last=cycle
        else:
            last=cycle
    if sent+epsilon < last:
        # Need to send the last group of cycles
        writeme='<cycledef>%s00 %s00 06:00:00</cycledef> ' \
                     %(first.strftime('%Y%m%d%H'),
                       last.strftime('%Y%m%d%H'))
        ctream.write(writeme)
    ctream.write('\n')

    out=str(ctream.getvalue())
    ctream.close()
    return out

